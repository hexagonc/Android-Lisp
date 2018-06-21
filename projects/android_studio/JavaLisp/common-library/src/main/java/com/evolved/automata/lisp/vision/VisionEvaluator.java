package com.evolved.automata.lisp.vision;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.vision.VisionTools;

import java.nio.ByteBuffer;

/**
 * Created by Evolved8 on 7/12/17.
 */

public class VisionEvaluator {

    public static void addVisionFunctions(Environment env)
    {
        //env.mapFunction("convert-rgb-byte-buffer-to-rgb-buffer", convertYUV422ByteBufferToRGBBuffer());


    }



/*
    static SimpleFunctionTemplate convertYUV422ByteBufferToRGBBuffer()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)convertYUV422ByteBufferToRGBBuffer();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(4, true, true);

                ByteBuffer buffer = (ByteBuffer)evaluatedArgs[0].getObjectValue();
                int height = (int)evaluatedArgs[1].getIntValue();
                int width = (int)evaluatedArgs[2].getIntValue();
                String byteOrder = evaluatedArgs[3].getString();

                VisionTools.ByteOrder order = VisionTools.ByteOrder.valueOf(byteOrder);

                byte[] converted = VisionTools.convertYUV422ToRGB(buffer.array(), height, width, order);

                ByteBuffer rgbBuffer = ByteBuffer.wrap(converted);
                return ExtendedFunctions.makeValue(rgbBuffer);
            }
        };
    }
    */


}
