package com.evolved.automata.android.media;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;

import com.evolved.automata.vision.VisionTools;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Created by Andrew Baughns on 7/11/2017.
 */
public class MediaTools {


    public static byte[] convertToPNGBytes(VisionTools.ARGB[] argbColors, int height, int width)
    {
        int[] colors = new int[argbColors.length];
        VisionTools.ARGB simpleARGB;
        for (int i = 0; i < colors.length;i++)
        {
            simpleARGB = argbColors[i];
            colors[i] = Color.argb(simpleARGB.a(), simpleARGB.r(), simpleARGB.b(), simpleARGB.g());
        }
        Bitmap map = Bitmap.createBitmap(colors, 0, width, width, height, Bitmap.Config.ARGB_8888);
        ByteArrayOutputStream baostream = new ByteArrayOutputStream();
        try
        {
            boolean success = map.compress(Bitmap.CompressFormat.PNG, 100, baostream);
            if (success)
                return baostream.toByteArray();
            else
                return null;
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }

    }


    public static byte[] convertToJPEGBytes(VisionTools.ARGB[] argbColors, int height, int width, int quality)
    {
        int[] colors = new int[argbColors.length];
        VisionTools.ARGB simpleARGB;
        for (int i = 0; i < colors.length;i++)
        {
            simpleARGB = argbColors[i];
            colors[i] = Color.argb(simpleARGB.a(), simpleARGB.r(), simpleARGB.b(), simpleARGB.g());
        }
        Bitmap map = Bitmap.createBitmap(colors, 0, width, width, height, Bitmap.Config.ARGB_8888);
        ByteArrayOutputStream baostream = new ByteArrayOutputStream();
        try
        {
            boolean success = map.compress(Bitmap.CompressFormat.JPEG, quality, baostream);
            if (success)
                return baostream.toByteArray();
            else
                return null;
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }

    }

    public static ByteBuffer convertRGBByteBufferToPNGBuffer(byte[] rawRGB, int height, int width, VisionTools.ByteOrder sourceByteOrder)
    {
        VisionTools.ARGB[] colors = VisionTools.convertRGBBytesToColor(rawRGB, height, width, sourceByteOrder);
        byte[] bytes = convertToPNGBytes(colors, height, width);
        return ByteBuffer.wrap(bytes);
    }


    public static ByteBuffer convertRGBByteBufferToJPEGBuffer(byte[] rawRGB, int height, int width, VisionTools.ByteOrder sourceByteOrder, int quality)
    {
        VisionTools.ARGB[] colors = VisionTools.convertRGBBytesToColor(rawRGB, height, width, sourceByteOrder);
        byte[] bytes = convertToJPEGBytes(colors, height, width, quality);
        return ByteBuffer.wrap(bytes);
    }
}
