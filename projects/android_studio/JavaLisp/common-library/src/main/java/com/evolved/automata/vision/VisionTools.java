package com.evolved.automata.vision;



/**
 * Created by Evolved8 on 7/5/17.
 */

public class VisionTools {

    public static String convertDataToBase64(byte[] data)
    {
        return org.apache.commons.codec.binary.Base64.encodeBase64String(data);
    }


}
