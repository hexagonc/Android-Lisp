package com.evolved.automata.vision.google;



import com.evolved.automata.vision.VisionTools;

import org.apache.commons.lang3.tuple.Pair;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashSet;

import javax.net.ssl.HttpsURLConnection;

/**
 * Created by Evolved8 on 7/5/17.
 */

public class GoogleVisionManager {

    public enum FeatureType
    {
        FACE_DETECTION,
        LOGO_DETECTION,
        LABEL_DETECTION,
        CROP_HINTS,
        LANDMARK_DETECTION,
        TEXT_DETECTION,
        IMAGE_PROPERTIES,
        WEB_DETECTION,
        DOCUMENT_TEXT_DETECTION
    }

    public static class Feature
    {
        FeatureType _type;
        int _maxResults;

        public FeatureType getType()
        {
            return _type;
        }

        public int getMaxResults()
        {
            return _maxResults;
        }

        Feature(FeatureType type, int resultCount)
        {
            _maxResults = resultCount;
            _type = type;
        }

    }

    public static class ImageAnnotationRequest
    {
        byte[] _image;
        Feature[] _features;
        String[] _languageHints;

        ImageAnnotationRequest(byte[] image, Feature[] features, String[] languageHints)
        {
            _image = image;
            _features = features;
            _languageHints = languageHints;
        }

        public byte[] getImageContent()
        {
            return _image;
        }

        public Feature[] getFeatures()
        {
            return _features;
        }

        public String[] getLanguageHints()
        {
            return _languageHints;
        }
    }


    public static final String BASE_REQUEST_URL = "https://vision.googleapis.com/v1/images:annotate?key=%1$s";



    public GoogleVisionManager()
    {

    }

    public static JSONObject makeHttpJSONRequest(String urlString, JSONObject request) throws IOException, JSONException
    {
        String requestString = request.toString();
        URL url = new URL(urlString);
        HttpsURLConnection connection = null;
        connection = (HttpsURLConnection)url.openConnection();
        connection.setDoInput(true);
        connection.setDoOutput(true);
        connection.setRequestMethod("POST");

        byte[] requestBody = requestString.getBytes("UTF-8");
        connection.addRequestProperty("Content-Type", "application/json");
        connection.addRequestProperty("Content-Length", "" + requestBody.length);
        OutputStream ostream = connection.getOutputStream();
        ostream.write(requestBody);
        ostream.flush();
        connection.connect();

        if (connection.getResponseCode() == HttpURLConnection.HTTP_OK)
        {
            try
            {
                InputStream istream = connection.getInputStream();
                StringBuilder builder = new StringBuilder();
                int charValue;
                while ((charValue = istream.read())!= -1)
                {
                    builder.appendCodePoint(charValue);
                }
                JSONObject result = new JSONObject(builder.toString());
                return result;
            }
            finally
            {
                connection.disconnect();
            }


        }
        else
        {
            throw new RuntimeException(connection.getResponseMessage());
        }

    }

    public static JSONObject requestImageData(ImageAnnotationRequest[] imageRequests, String apiKey) throws IOException, JSONException
    {
        JSONObject request = getImageAnnotationRequests(imageRequests);

        String urlString = getVisionUrl(BASE_REQUEST_URL, apiKey);

        return makeHttpJSONRequest(urlString, request);

    }


    public static HashSet<Pair<String, Double>> getVisibleLabels(JSONObject imageLabelResult) throws JSONException
    {
        HashSet<Pair<String, Double>> result = new HashSet<Pair<String, Double>>();
        JSONArray responses = imageLabelResult.getJSONArray("responses");
        int numResponses = responses.length();
        for (int i = 0; i < numResponses;i++)
        {
            JSONObject response = responses.getJSONObject(i);
            JSONArray annotations = response.getJSONArray("labelAnnotations");
            int numAnnotations = annotations.length();
            for (int j = 0; j < numAnnotations;j++)
            {
                JSONObject annotation = annotations.getJSONObject(j);
                double score = annotation.getDouble("score");
                String desc = annotation.getString("description");
                result.add(Pair.of(desc, Double.valueOf(score)));
            }

        }
        return result;
    }

    public static String getAllVisibleText(JSONObject imageTextResult) throws JSONException
    {
        JSONArray responses = imageTextResult.getJSONArray("responses");
        int numResponses = responses.length();
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < numResponses;i++)
        {
            JSONObject response = responses.getJSONObject(i);
            JSONObject fullTextAnnotations = response.getJSONObject("fullTextAnnotation");
            builder.append(fullTextAnnotations.getString("text"));
        }
        return builder.toString();
    }

    public static ImageAnnotationRequest getImageAnnotationRequest(byte[] image, Feature[] features, String[] languageHints)
    {
        return new ImageAnnotationRequest(image, features, languageHints);
    }


    public static Feature makeImageFeature(FeatureType type, int maxResults)
    {
        return new Feature(type, maxResults);
    }

    private static JSONObject getImageAnnotationRequests(ImageAnnotationRequest[] imageRequests) throws JSONException
    {
        JSONObject obj = new JSONObject();
        JSONArray array = new JSONArray();

        for (int i = 0; i < imageRequests.length;i++)
        {
            array.put(i, makeAnnotateImageRequest(imageRequests[i]));
        }
        obj.put("requests", array);
        return obj;
    }


    private static String getVisionUrl(String baseUrlPattern, String apiKey)
    {
        return String.format(baseUrlPattern, apiKey);
    }




    private static JSONObject makeAnnotateImageRequest(ImageAnnotationRequest imageRequest) throws JSONException
    {
        return makeAnnotateImageRequest(imageRequest.getImageContent(), imageRequest.getFeatures(), imageRequest.getLanguageHints());
    }


    private static JSONObject makeAnnotateImageRequest(byte[] image, Feature[] features, String[] languageHints) throws JSONException
    {
        JSONObject obj = new JSONObject();
        obj.put("image", makeImage(image));
        JSONArray array = new JSONArray();

        int i = 0;
        for (Feature feature:features)
        {
            array.put(i, makeFeature(feature.getType(), feature.getMaxResults()));
            i++;
        }
        obj.put("features", array);
        obj.put("imageContext", makeImageContext(languageHints));
        return obj;
    }

    private static JSONObject makeFeature(FeatureType type, int maxResults) throws JSONException
    {
        JSONObject obj = new JSONObject();

        obj.put("type", type.name());
        obj.put("maxResults", maxResults);
        return obj;
    }

    private static JSONObject makeImage(byte[] image) throws JSONException
    {
        JSONObject obj = new JSONObject();
        String imageB64 = VisionTools.convertDataToBase64(image);
        obj.put("content", imageB64);
        return obj;
    }

    private static JSONObject makeImageContext(String[] languageHints) throws JSONException
    {
        JSONObject obj = new JSONObject();

        JSONArray hints = new JSONArray();
        int i = 0;
        for (String lang:languageHints)
        {
            hints.put(i, lang);
            i++;
        }

        obj.put("languageHints", hints);

        return obj;
    }

}
