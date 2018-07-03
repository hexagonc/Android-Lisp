package com.evolved.automata;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.function.Predicate;

public class ConcurrentGenerator <Result> {

    Predicate<Result> _resultTester = null;
    HashMap<InterruptibleResultProducer<Result>, Thread> _processMap;

    CountDownLatch _latch;

    volatile Result _final;

    Exception _exception = null;

    boolean _failOnAnyExceptionP = false;

    Object _synch = new Object();

    public int updateCount = 0;
    private volatile boolean _finishedP = false;

    public ConcurrentGenerator(){
        _processMap = new HashMap<>();
    }

    public ConcurrentGenerator(Predicate<Result> acceptancePredicate){
        _processMap = new HashMap<>();
        _resultTester = acceptancePredicate;
    }

    public Exception getException(){
        return _exception;
    }

    public ConcurrentGenerator<Result> setFailOnAnyError(boolean enabled){
        _failOnAnyExceptionP = enabled;
        return this;
    }

    public ConcurrentGenerator<Result> addSupplier(final InterruptibleResultProducer<Result> sup){
        _processMap.put(sup, new Thread(){
            public void run(){

                try
                {
                    synchronized (_synch){
                        updateCount++;
                        if (_finishedP){
                            return;
                        }
                    }
                    sup.computeResult();
                    setResult(sup);

                }
                catch (Exception e){
                    _latch.countDown();
                    _exception = e;

                    if (_failOnAnyExceptionP){
                        finishAll();
                    }
                }

            }
        });
        return this;
    }

    private void finishAll(){
        synchronized (_synch){
            for (Map.Entry<InterruptibleResultProducer<Result>, Thread> entry:_processMap.entrySet()){
                entry.getValue().interrupt();
            }
            _finishedP = true;
        }

    }

    private void setResult(InterruptibleResultProducer<Result> source){
        synchronized (_synch){
            if (_finishedP){
                return;
            }

            if (_resultTester != null){
                _finishedP = _resultTester.test(source.getResult());
            }
            else
                _finishedP = true;

            if (_finishedP || updateCount == _processMap.size()){
                for (Map.Entry<InterruptibleResultProducer<Result>, Thread> entry:_processMap.entrySet()){
                    if (entry.getKey() != source){
                        entry.getValue().interrupt();
                    }
                }
                _final = source.getResult();
                _latch.countDown();
            }

        }
    }

    public boolean isResultValid(){
        return _finishedP;
    }


    public Result getResult(){
        _final = null;
        if (_processMap.size() <= 1 ) {

            for (Map.Entry<InterruptibleResultProducer<Result>, Thread> entry:_processMap.entrySet()){
                _final =  entry.getKey().computeResult().getResult();
            }
            if (_resultTester != null && _final != null)
                _finishedP = _resultTester.test(_final);
            return _final;
        }

        _latch = new CountDownLatch(1);
        for (Map.Entry<InterruptibleResultProducer<Result>, Thread> entry:_processMap.entrySet()){
            entry.getValue().start();
        }
        try
        {
            _latch.await();
        } catch (InterruptedException e)
        {
            e.printStackTrace();
        }
        _processMap.clear();
        return _final;
    }
}
