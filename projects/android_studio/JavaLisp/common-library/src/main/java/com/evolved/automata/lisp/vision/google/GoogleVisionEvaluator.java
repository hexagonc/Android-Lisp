package com.evolved.automata.lisp.vision.google;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.vision.google.GoogleVisionManager;

import org.apache.commons.lang3.tuple.Pair;
import org.json.JSONObject;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 7/13/17.
 */

public class GoogleVisionEvaluator {
    public static void addVisionFunctions(Environment env)
    {
        env.mapFunction("get-google-vision-png-result", getSpecificGoogleVisionAPIResult());


    }




    static SimpleFunctionTemplate getSpecificGoogleVisionAPIResult()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)getSpecificGoogleVisionAPIResult();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(4, true, true);

                ByteBuffer pngBuffer = (ByteBuffer)evaluatedArgs[0].getObjectValue();
                String feature = evaluatedArgs[1].getString();
                int numResults = (int)evaluatedArgs[2].getIntValue();
                String apiKey = evaluatedArgs[3].getString();
                Value response = Environment.getNull();
                try
                {
                    GoogleVisionManager.Feature[] features = new GoogleVisionManager.Feature[]{
                            GoogleVisionManager.makeImageFeature(GoogleVisionManager.FeatureType.valueOf(feature), numResults)
                    };


                    byte[] image = pngBuffer.array();

                    GoogleVisionManager.ImageAnnotationRequest[] annotationRequest = new GoogleVisionManager.ImageAnnotationRequest[]
                            {
                                    GoogleVisionManager.getImageAnnotationRequest(image, features, new String[]{"en"})
                            };

                    JSONObject imageResult = GoogleVisionManager.requestImageData(annotationRequest, apiKey);

                    if (feature.equals("LABEL_DETECTION"))
                    {

                        HashSet<Pair<String, Double>> result = GoogleVisionManager.getVisibleLabels(imageResult);

                        HashMap<String, Value> resultMap = new HashMap<String, Value>();
                        for (Pair<String, Double> v:result)
                        {
                            resultMap.put(v.getKey(), NLispTools.makeValue(v.getValue().doubleValue()));
                        }
                        response = new StringHashtableValue(resultMap);

                    }
                    else if (feature.equals("TEXT_DETECTION"))
                    {
                        String imageText = GoogleVisionManager.getAllVisibleText(imageResult);
                        response = NLispTools.makeValue(imageText);
                    }
                    else
                    {
                        response = NLispTools.makeValue(imageResult.toString());
                    }
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }

                return response;

            }
        };
    }




}
