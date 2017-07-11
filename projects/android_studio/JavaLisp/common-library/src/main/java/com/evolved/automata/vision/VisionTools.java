package com.evolved.automata.vision;


import java.nio.ByteBuffer;

/**
 * Created by Evolved8 on 7/5/17.
 */

public class VisionTools {

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
