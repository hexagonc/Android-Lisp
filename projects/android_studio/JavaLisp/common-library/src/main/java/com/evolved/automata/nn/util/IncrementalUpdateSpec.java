package com.evolved.automata.nn.util;

import com.evolved.automata.lisp.nn.LSTMNetworkProxy;
import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by Evolved8 on 5/28/18.
 */

public class IncrementalUpdateSpec {

    public static final double FRACTIONAL_ERROR = 0.000001;
    public LSTMNetworkProxy _bestNetwork;
    public LSTMNetworkProxy.NodeState _initialState;
    public LSTMNetworkProxy.NodeState _finalState;
    public float _successFraction;

    public boolean _isValid = false;
    public boolean _successCriteriaSatisfiedP = false;
    int _length = 0;
    Double mMinValue = null;

    IncrementalUpdateSpec(){

    }

    public IncrementalUpdateSpec(boolean isValid, boolean successCriteriaSatisfiedP, LSTMNetworkProxy bestNetwork, boolean copyNetworkP, LSTMNetworkProxy.NodeState initialState, LSTMNetworkProxy.NodeState finalState, float successFraction){
        if (copyNetworkP) {
            _bestNetwork = bestNetwork.duplicate(bestNetwork);
        }
        else {
            _bestNetwork = bestNetwork;
        }
        _isValid = isValid;
        _successCriteriaSatisfiedP = successCriteriaSatisfiedP;
        _initialState = initialState;
        _finalState = finalState;
        _successFraction = successFraction;
    }

    public byte[] serializeBytes(){
        return GroupSerializer.get().serialize()
                .add(LSTMNetworkProxy.class, _bestNetwork)
                .add(LSTMNetworkProxy.NodeState.class, _initialState)
                .add(LSTMNetworkProxy.NodeState.class, _finalState)
                .add(Float.valueOf(_successFraction))
                .add(Boolean.valueOf(_isValid))
                .add(Boolean.valueOf(_successCriteriaSatisfiedP))
                .add(Integer.valueOf(_length))
                .add(Double.class, mMinValue).build();
    }

    public static IncrementalUpdateSpec deserializeBytes(byte[] b){
        IncrementalUpdateSpec spec = new IncrementalUpdateSpec();
        ArrayList components = GroupSerializer.get().deserialize(b);
        spec._bestNetwork = (LSTMNetworkProxy)components.get(0);
        spec._initialState = (LSTMNetworkProxy.NodeState)components.get(1);
        spec._finalState = (LSTMNetworkProxy.NodeState)components.get(2);
        spec._successFraction = (Float)components.get(3);
        spec._isValid = (Boolean)components.get(4);
        spec._successCriteriaSatisfiedP = (Boolean)components.get(5);
        spec._length = (Integer)components.get(6);
        spec.mMinValue = (Double)components.get(7);
        return spec;
    }


    public IncrementalUpdateSpec copyUsing(IncrementalUpdateSpec other){
        float[] otherNetwork = other.getNetwork().getNetwork();
        float[] myNetwork = _bestNetwork.getNetwork();
        if (_bestNetwork == null || _initialState == null || _finalState == null)
        {
            // DO NOTHING
            return other;
        }

        for (int i = 0;i<myNetwork.length;i++){
            otherNetwork[i] = myNetwork[i];
        }

        LSTMNetworkProxy.NodeState initialState, finalState;

        float[] otherInitial = null;
        float[] otherFinal = null;

        float[] myInitial = _initialState.getRawActivations();
        float[] myFinal = _finalState.getRawActivations();

        if (other._initialState != null && other._initialState.getRawActivations().length == _initialState.getRawActivations().length)
            otherInitial = other._initialState.getRawActivations();
        else
            otherInitial = new float[myInitial.length];

        if (other._finalState != null && other._finalState.getRawActivations().length == _finalState.getRawActivations().length)
            otherFinal = other._finalState.getRawActivations();
        else
            otherFinal = new float[myFinal.length];

        for (int i = 0; i < otherInitial.length;i++){
            otherInitial[i] = myInitial[i] ;
            otherFinal[i] = myFinal[i];
        }
        initialState = LSTMNetworkProxy.NodeState.createNodeState(otherInitial);
        finalState = LSTMNetworkProxy.NodeState.createNodeState(otherFinal);

        IncrementalUpdateSpec spec = new IncrementalUpdateSpec(_isValid, _successCriteriaSatisfiedP, LSTMNetworkProxy.make(otherNetwork), false, initialState, finalState, _successFraction);
        spec.setLength(_length);
        return spec;
    }

    public boolean shiftForward(){
        if (_bestNetwork != null &&
                _initialState != null &&
                _finalState != null &&
                !_initialState.compare(_finalState, 0)){
            _bestNetwork.setNodeState(_initialState);
            float[] out = _bestNetwork.getOutputVaues();
            _bestNetwork.executeForwardPass(NNTools.roundToInt(out));
            _initialState = _bestNetwork.getCurrentNodeState();
            _length--;
            return true;
        }
        return false;
    }

    public int getLength(){
        return _length;
    }

    public IncrementalUpdateSpec setLength(int len){
        _length = len;
        return this;
    }

    public boolean isValid(){
        return _isValid;
    }

    public boolean successCriteriaSatisfied(){
        return _successCriteriaSatisfiedP;
    }

    public LSTMNetworkProxy getNetwork(){
        return _bestNetwork;
    }

    public Double getDistanceToEnd(){
        if (_finalState == null)
            return null;
        return LSTMNetworkProxy.getAverageError(_bestNetwork.getCurrentNodeState(), _finalState);
    }

    public Double getFractionalDistanceToEnd(){
        if (_finalState == null)
            return null;
        if (mMinValue == null){
            float[] f = _finalState.getRawActivations();
            double m = 0, s = 0, v;
            for (int i = 0;i<f.length;i++){
                v = Math.abs(f[i]);
                if (i == 0)
                    m = v;
                else if (i == 1){
                    m = Math.min(m, v);
                    s = Math.max(m, v);
                }
                else {
                    if (v < s)
                    {
                        m = Math.min(m, v);
                        s = Math.max(m, v);
                    }
                }
            }

            if (m == 0)
                mMinValue = FRACTIONAL_ERROR;
            else
                mMinValue = Double.valueOf(0);
        }
        return LSTMNetworkProxy.getAverageFractionalError(_bestNetwork.getCurrentNodeState(), _finalState, mMinValue);
    }

    public ArrayList<Vector> extrapolateRange(){
        return extrapolateRange(false);
    }

    public ArrayList<Vector> extrapolateRange(boolean includeInitialValue){
        ArrayList<Vector> out = new ArrayList<Vector>();

        _bestNetwork.setNodeState(_initialState);
        float[] v;
        if (includeInitialValue){
            v = NNTools.roundToInt(_bestNetwork.getLastInput());
            out.add(new Vector(v));
        }
        while (!_bestNetwork.getCurrentNodeState().compare(_finalState, 0)){
            v = NNTools.roundToInt(_bestNetwork.getOutputVaues());
            out.add(new Vector(v));

            _bestNetwork.executeForwardPass(v);
        }
        v = NNTools.roundToInt(_bestNetwork.getOutputVaues());
        out.add(new Vector(v));
        return out;
    }

    public void setInitialState(){
        if (_initialState != null){
            _bestNetwork.setNodeState(_initialState);
        }
    }

}
