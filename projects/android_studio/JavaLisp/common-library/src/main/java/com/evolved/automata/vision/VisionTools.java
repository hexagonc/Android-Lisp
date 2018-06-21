package com.evolved.automata.vision;


import java.nio.ByteBuffer;

/**
 * Created by Evolved8 on 7/5/17.
 */

public class VisionTools {

    public static class ARGB
    {
        int a;
        int r;
        int g;
        int b;

        public ARGB(int aa, int rr, int gg, int bb)
        {
            a=aa;
            r =rr;
            g = gg;
            b = bb;
        }

        public int a()
        {
            return a;
        }

        public int r()
        {
            return r;
        }

        public int g()
        {
            return g;
        }

        public int b()
        {
            return b;
        }
    }

    public enum ByteOrder
    {
        RGB_888, BGR_888, RGB_8888, BGR_8888
    }


    private static int byteToUnsignedInt(byte b)
    {
        if (b >= 0)
            return b;
        else
            return b + 256;
    }

    public static ARGB[] convertRGBBytesToColor(byte[] input, int height, int width,  ByteOrder sourceByteOrder)
    {
        int totalLength = input.length;
        int srOffset = 2, sgOffset = 1, sbOffset = 0;

        ARGB[] rgb = new ARGB[height*width];

        int alpha = 255;
        switch (sourceByteOrder)
        {
            case RGB_8888:
            case RGB_888:
                sbOffset = 0;
                sgOffset = 1;
                srOffset = 2;
                break;
            case BGR_8888:
            case BGR_888:
                srOffset = 0;
                sgOffset = 1;
                sbOffset = 2;
                break;
        }


        int rgbIndex = 0;
        for (int i = 0; i < totalLength;i+=3)
        {
            rgb[rgbIndex] = new ARGB(alpha, byteToUnsignedInt(input[i + srOffset]), byteToUnsignedInt(input[i + sgOffset]), byteToUnsignedInt(input[i + sbOffset]));
            rgbIndex++;
        }
        return rgb;
    }


    public static int[] convertRGBBytesToInts(byte[] input, int height, int width,  ByteOrder sourceByteOrder, ByteOrder targetByteOrder)
    {
        int totalLength = input.length;
        int srOffset = 2, sgOffset = 1, sbOffset = 0;
        int trOffset=0, tgOffset=0, tbOffset=0, taOffset=0;
        int numBytes = 0;

        switch (targetByteOrder)
        {
            case RGB_8888:
                numBytes = 4;
                tbOffset = 0;
                tgOffset = 1;
                trOffset = 2;
                taOffset = 3;
                break;
            case RGB_888:
                numBytes = 3;
                tbOffset = 0;
                tgOffset = 1;
                trOffset = 2;
                break;
            case BGR_888:
                numBytes = 3;
                trOffset = 0;
                tgOffset = 1;
                tbOffset = 2;
                break;
            case BGR_8888:
                numBytes = 4;
                trOffset = 0;
                tgOffset = 1;
                tbOffset = 2;
                taOffset = 3;
                break;
        }


        int[] rgb = new int[numBytes*height*width];

        int alpha = 255;
        switch (sourceByteOrder)
        {
            case RGB_8888:
            case RGB_888:
                sbOffset = 0;
                sgOffset = 1;
                srOffset = 2;
                break;
            case BGR_8888:
            case BGR_888:
                srOffset = 0;
                sgOffset = 1;
                sbOffset = 2;
                break;
        }




        int rgbIndex = 0;
        for (int i = 0; i < totalLength;i+=3)
        {
            if (input[i + srOffset] < 0)
            {

            }
            rgb[rgbIndex + trOffset] = byteToUnsignedInt(input[i + srOffset]);
            rgb[rgbIndex + tgOffset] = byteToUnsignedInt(input[i + sgOffset]);
            rgb[rgbIndex + tbOffset] = byteToUnsignedInt(input[i + sbOffset]);

            if (numBytes == 4)
            {
                rgb[rgbIndex + taOffset] = alpha;
            }

            rgbIndex+=numBytes;
        }
        return rgb;
    }



    public static int[] convertYUV422BytesToInts(byte[] input, int height, int width,  ByteOrder targetByteOrder)
    {
        int totalLength = input.length;
        int trOffset=0, tgOffset=0, tbOffset=0, taOffset=0;
        int numBytes = 0;

        switch (targetByteOrder)
        {
            case RGB_8888:
                numBytes = 4;
                tbOffset = 0;
                tgOffset = 1;
                trOffset = 2;
                taOffset = 3;
                break;
            case RGB_888:
                numBytes = 3;
                tbOffset = 0;
                tgOffset = 1;
                trOffset = 2;
                break;
            case BGR_888:
                numBytes = 3;
                trOffset = 0;
                tgOffset = 1;
                tbOffset = 2;
                break;
            case BGR_8888:
                numBytes = 4;
                trOffset = 0;
                tgOffset = 1;
                tbOffset = 2;
                taOffset = 3;
                break;
        }


        int[] rgb = new int[numBytes*height*width];

        int alpha = 255;
        int c1, d1, e1, c2, d2, e2;
        int y1Offset = 0, vOffset = 1, y2Offset = 2, uOffset = 3;

        int r1, r2, g1,g2,b1,b2, y1,y2, u,v;
        int rgbIndex = 0;
        for (int i = 0; i < totalLength;i+=4)
        {
            u = byteToUnsignedInt(input[i + uOffset]);
            y1 = byteToUnsignedInt(input[i + y1Offset]);
            v = byteToUnsignedInt(input[i + vOffset]);
            y2 = byteToUnsignedInt(input[i + y2Offset]);

            c1 = (y1 - 16);
            c2 =(y2 - 16);

            d1 = (u - 128);
            d2 = d1;

            e1 = (v - 128);
            e2 = e1;


            r1 = Math.max(0, Math.min(255, ((298*c1 + 409*e1 + 128) >>> 8)));
            g1 = Math.max(0, Math.min(255, ((298*c1 - 100*d1 - 208*e1 + 128) >>>8 )));
            b1 = Math.max(0, Math.min(255, ((298*c1 - 516*d1 + 128) >>>8 )));

            r2 = Math.max(0, Math.min(255, ((298*c2 + 409*e2 + 128) >>> 8)));
            g2 = Math.max(0, Math.min(255, ((298*c2 - 100*d2 - 208*e2 + 128) >>>8 )));
            b2 = Math.max(0, Math.min(255, ((298*c2 - 516*d2 + 128) >>>8 )));


            rgb[rgbIndex + trOffset] = r1;
            rgb[rgbIndex + tgOffset] = g1;
            rgb[rgbIndex + tbOffset] = b1;

            if (numBytes == 4)
            {
                rgb[rgbIndex + taOffset] = alpha;
                rgb[rgbIndex + taOffset + numBytes] = alpha;
            }
            rgb[rgbIndex + trOffset + numBytes] = r2;
            rgb[rgbIndex + tgOffset + numBytes] = g2;
            rgb[rgbIndex + tbOffset + numBytes] = b2;

            rgbIndex+=numBytes*2;
        }
        return rgb;
    }


    public static String convertDataToBase64(byte[] data)
    {
        return org.apache.commons.codec.binary.Base64.encodeBase64String(data);
    }

    public static String convertDataToBase64(ByteBuffer buffer)
    {
        return convertDataToBase64(buffer.array());
    }

    public static ByteBuffer convertBase64ToByteBuffer(String base64)
    {
        byte[] bytes = org.apache.commons.codec.binary.Base64.decodeBase64(base64);
        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        return buffer;
    }


}
