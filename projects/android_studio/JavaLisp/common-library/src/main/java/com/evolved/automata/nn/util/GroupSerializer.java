package com.evolved.automata.nn.util;

import com.evolved.automata.lisp.nn.LSTMNetworkProxy;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;

public class GroupSerializer {

    public enum TYPE {
        STRING, INTEGER, FLOAT, DOUBLE, FEATURE_MODEL, LSTM, NODE_STATE
    }


    public class Builder {
        ArrayList<ByteBuffer> _data = null;
        int _total = 0;
        Builder(){
            _data = new ArrayList<>();
        }

        public Builder add(Object o){
            return add(o.getClass(), o);
        }

        public Builder add(Class c, Object o){

            Triple<Integer, Serializer, Deserializer> spec = mConversionMap.get(c);
            int typeIndex = spec.getLeft();
            Serializer serializer = spec.getMiddle();
            byte[] data = new byte[0];
            if (o != null){
                data = serializer.serialize(o);
            }

            ByteBuffer buffer =  ByteBuffer.allocate(5 + data.length);
            _total+=5+data.length;
            buffer.put((byte)typeIndex);
            if (o == null)
                buffer.putInt(-1);
            else
                buffer.putInt(data.length);

            if (data.length > 0){
                buffer.put(data);
            }

            _data.add(buffer);
            return this;
        }

        public byte[] build(){
            byte[] out = new byte[_total];
            int offset = 0;
            for (ByteBuffer b:_data){
                byte[] buffer = b.array();
                for (int i = offset;i < buffer.length+offset;i++){
                    out[i]=buffer[i-offset];
                }
                offset = offset + buffer.length;
            }
            return out;
        }
    }

    HashMap<Class, Triple<Integer, Serializer, Deserializer>> mConversionMap;

    static GroupSerializer mSerializer = null;

    ArrayList<Class> mConversion;

    GroupSerializer(){
        mConversion = new ArrayList<>();
        mConversionMap = new HashMap<>();
        registerDefaultSerializers();
    }

    public static GroupSerializer get(){
        if (mSerializer == null){
            mSerializer = new GroupSerializer();
        }
        return mSerializer;
    }

    public Builder serialize(){
        return new Builder();
    }

    public GroupSerializer registerSerializer(Class c, Serializer serializer, Deserializer deserializer){
        int index = mConversionMap.size();
        mConversion.add(c);
        mConversionMap.put(c, Triple.of(Integer.valueOf(index), serializer, deserializer));
        return this;
    }

    private void registerDefaultSerializers(){

        // Floats
        registerSerializer(Float.class, (Object o) -> {
            Float f = (Float)o;

            ByteBuffer b = ByteBuffer.allocate(4);
            b.putFloat(f);
            return b.array();
        }, (byte[] b)->{
           ByteBuffer buffer = ByteBuffer.wrap(b);
           return buffer.getFloat();
        });

        // Integers
        registerSerializer(Integer.class, (Object o) -> {
            Integer integer = (Integer)o;

            ByteBuffer b = ByteBuffer.allocate(4);
            b.putInt(integer);
            return b.array();
        }, (byte[] b)->{
            ByteBuffer buffer = ByteBuffer.wrap(b);
            return buffer.getInt();
        });

        // Double
        registerSerializer(Double.class, (Object o) -> {
            Double d = (Double)o;

            ByteBuffer b = ByteBuffer.allocate(8);
            b.putDouble(d);
            return b.array();
        }, (byte[] b)->{
            ByteBuffer buffer = ByteBuffer.wrap(b);
            return buffer.getDouble();
        });

        // Long
        registerSerializer(Long.class, (Object o) -> {
            Long l = (Long)o;

            ByteBuffer b = ByteBuffer.allocate(8);
            b.putDouble(l);
            return b.array();
        }, (byte[] b)->{
            ByteBuffer buffer = ByteBuffer.wrap(b);
            return buffer.getLong();
        });

        // Strings
        registerSerializer(String.class, (Object o) -> {
            try
            {
                String str = (String)o;

                return str.getBytes("UTF-8");
            }
            catch(Exception e){
                throw new RuntimeException(e);
            }

        }, (byte[] b)->{
            try
            {
                if (b.length == 0)
                    return "";
                return new String(b, "UTF-8");
            }
            catch (Exception e){
                throw new RuntimeException(e);
            }

        });

        // Boolean
        registerSerializer(Boolean.class, (Object o) -> {
            Boolean bool = (Boolean)o;

            byte[] b = new byte[1];
            if (bool.booleanValue()){
                b[0] = (byte)1;
            }
            else {
                b[0] = (byte)0;
            }
            return b;
        }, (byte[] b)->{
            int v = b[0];
            return Boolean.valueOf(v == 1);
        });


        // LSTMNetworkProxy
        registerSerializer(LSTMNetworkProxy.class, (Object o) -> {
            LSTMNetworkProxy lstm = (LSTMNetworkProxy)o;

            return lstm.serializeBytes();
        }, (byte[] b)->{

            return LSTMNetworkProxy.deserializeBytes(b);
        });

        // LSTMNodeState
        registerSerializer(LSTMNetworkProxy.NodeState.class, (Object o) -> {
            LSTMNetworkProxy.NodeState nodestate = (LSTMNetworkProxy.NodeState)o;

            return nodestate.serializeBytes();
        }, (byte[] b)->{

            return LSTMNetworkProxy.NodeState.deserializeBytes(b);
        });

        registerSerializer(FeatureModel.Similarity.class,
                (Object o)->((FeatureModel.Similarity) o).serializeBytes(),
                (byte[] b)->FeatureModel.Similarity.deserializeBytes(b));

        registerSerializer(FeatureModel.SimilaryHistory.class,
                (Object o)->((FeatureModel.SimilaryHistory) o).serializeBytes(),
                (byte[] b)->FeatureModel.SimilaryHistory.deserializeBytes(b));

        registerSerializer(LearningConfiguration.class,
                (Object o)->((LearningConfiguration) o).serializeBytes(),
                (byte[] b)->LearningConfiguration.deserializeBytes(b));

        registerSerializer(FeatureModel.class,
                (Object o)->((FeatureModel) o).serializeBytes(),
                (byte[] b)->FeatureModel.deserializeBytes(b));

        registerSerializer(IncrementalUpdateSpec.class,
                (Object o)->((IncrementalUpdateSpec) o).serializeBytes(),
                (byte[] b)->IncrementalUpdateSpec.deserializeBytes(b));

        registerSerializer(ByteBuffer.class,
                (Object o)->((ByteBuffer)o).array(),
                (byte[] b)->ByteBuffer.wrap(b));

        registerSerializer(Group.FeatureValueMetadata.class,
                (Object o)->((Group.FeatureValueMetadata)o).serializeBytes(),
                (byte[] b)->Group.FeatureValueMetadata.deserializeBytes(b));

        registerSerializer(Group.FeatureMetaData.class,
                (Object o)->((Group.FeatureMetaData)o).serializeBytes(),
                (byte[] b)->Group.FeatureMetaData.deserializeBytes(b));

        registerSerializer(WorldModel.GroupType.class,
                (Object o)->((WorldModel.GroupType)o).serializeBytes(),
                (byte[] b)->WorldModel.GroupType.deserializeBytes(b));

        registerSerializer(Group.class,
                (Object o)->((Group)o).serializeBytes(),
                (byte[] b)->Group.deserializeBytes(b, null));

    }

    public ArrayList deserialize(byte[] groupSerialization){
        ByteBuffer buffer = ByteBuffer.wrap(groupSerialization);

        ArrayList out = new ArrayList();

        int offset = 0;
        while (offset < groupSerialization.length){
            int typeIndex = (int)buffer.get();
            int size = buffer.getInt();

            if (size < 0){
                out.add(null);
            }
            else {
                byte[] data = new byte[size];
                for (int j = 0;j < size;j++){
                    data[j] = buffer.get();
                }
                Triple<Integer, Serializer, Deserializer> spec = mConversionMap.get(mConversion.get(typeIndex));
                Deserializer des = spec.getRight();
                Object value = des.deserialize(data);
                out.add(value);
            }

            offset = offset + 5 + Math.max(0, size);
        }
        return out;
    }


}
