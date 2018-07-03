package com.evolved.automata;

public abstract class InterruptibleResultProducer<T> {

    T result;

    public abstract T evaluate();

    public InterruptibleResultProducer<T> computeResult(){
        result = evaluate();
        return this;
    }

    public T getResult(){
        return result;
    }
}
