package com.evolved.automata.lisp;

import com.evolved.automata.filetools.StandardTools;

import java.io.IOException;

/**
 * Created by Evolved8 on 9/9/16.
 */
public class FileFunctions {
    public static Environment addFunctions(Environment env)
    {
        env.mapFunction("load", new SimpleFunctionTemplate() {
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                String fileName = evaluatedArgs[0].getString();
                try
                {
                    String[] lines = StandardTools.getDataFileLines(fileName);
                    env.getRootEnvironment().loadFromFileLines(lines);
                }
                catch (IOException ie)
                {
                    throw new RuntimeException(ie);
                }



                return null;
            }
        });
        return env;
    }



}
