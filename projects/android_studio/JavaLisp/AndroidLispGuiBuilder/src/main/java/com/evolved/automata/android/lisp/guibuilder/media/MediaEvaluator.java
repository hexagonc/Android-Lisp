package com.evolved.automata.android.lisp.guibuilder.media;

import com.evolved.automata.android.media.MediaTools;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.vision.VisionTools;

import java.nio.ByteBuffer;

/**
 * Created by Evolved8 on 7/13/17.
 */

public class MediaEvaluator {
    public static void addVisionFunctions(Environment env)
    {
        env.mapFunction("encode-byte-buffer-as-base64-string", getByteBufferAsString());
        env.mapFunction("decode-base64-string-to-byte-buffer", decodeByteBufferFromBase64());

        env.mapFunction("convert-rgb-byte-buffer-to-png-byte-buffer", convertRGBByteBufferToPNGByteBuffer());
        env.mapFunction("convert-rgb-byte-buffer-to-jpeg-byte-buffer", convertRGBByteBufferToJPEGByteBuffer());
    }

    static SimpleFunctionTemplate convertRGBByteBufferToJPEGByteBuffer()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)convertRGBByteBufferToJPEGByteBuffer();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(4, true, true);

                ByteBuffer buffer = (ByteBuffer)evaluatedArgs[0].getObjectValue();
                int height = (int)evaluatedArgs[1].getIntValue();
                int width = (int)evaluatedArgs[2].getIntValue();

                String sourceByteOrder = VisionTools.ByteOrder.RGB_888.name();


                int quality = 7;
                if (evaluatedArgs.length > 3)
                {
                    quality = (int)evaluatedArgs[3].getIntValue();
                    if (evaluatedArgs.length > 4)
                    {
                        sourceByteOrder = evaluatedArgs[4].getString();
                    }
                }


                byte[] rgbBytes = buffer.array();

                ByteBuffer jpgBuffer = MediaTools.convertRGBByteBufferToJPEGBuffer(rgbBytes, height, width, VisionTools.ByteOrder.valueOf(sourceByteOrder), quality);

                return ExtendedFunctions.makeValue(jpgBuffer);
            }
        };
    }

    static SimpleFunctionTemplate convertRGBByteBufferToPNGByteBuffer()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)convertRGBByteBufferToPNGByteBuffer();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(3, true, true);

                ByteBuffer buffer = (ByteBuffer)evaluatedArgs[0].getObjectValue();
                int height = (int)evaluatedArgs[1].getIntValue();
                int width = (int)evaluatedArgs[2].getIntValue();

                String sourceByteOrder = VisionTools.ByteOrder.RGB_888.name();

                if (evaluatedArgs.length > 3)
                {
                    sourceByteOrder = evaluatedArgs[3].getString();

                }


                byte[] rgbBytes = buffer.array();

                ByteBuffer pngBuffer = MediaTools.convertRGBByteBufferToPNGBuffer(rgbBytes, height, width, VisionTools.ByteOrder.valueOf(sourceByteOrder));

                return ExtendedFunctions.makeValue(pngBuffer);
            }
        };
    }




    static SimpleFunctionTemplate getByteBufferAsString()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)getByteBufferAsString();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                ByteBuffer buffer = (ByteBuffer)evaluatedArgs[0].getObjectValue();
                String stringForm = VisionTools.convertDataToBase64(buffer);

                return NLispTools.makeValue(stringForm);
            }
        };
    }

    static SimpleFunctionTemplate decodeByteBufferFromBase64()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)decodeByteBufferFromBase64();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String base64 = evaluatedArgs[0].getString();
                ByteBuffer buffer = VisionTools.convertBase64ToByteBuffer(base64);

                return ExtendedFunctions.makeValue(buffer);
            }
        };
    }
}
