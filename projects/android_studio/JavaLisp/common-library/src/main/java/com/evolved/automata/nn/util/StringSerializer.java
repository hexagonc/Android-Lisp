package com.evolved.automata.nn.util;

public interface StringSerializer {
    String serialize(Object o);
    Object deserialize(String data);
}
