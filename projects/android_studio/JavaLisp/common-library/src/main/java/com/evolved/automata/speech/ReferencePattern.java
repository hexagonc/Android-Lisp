package com.evolved.automata.speech;

import org.apache.commons.lang3.StringUtils;

import java.util.LinkedList;

public class ReferencePattern extends Pattern {

    public static final String COMP_DELIMITER = ":";

    NLUBuilder mBuilder;
    String mReferenceName;
    private ReferencePattern(NLUBuilder builder, String fullName)
    {
        mBuilder = builder;

        mReferenceName = getReferenceName(fullName);
        mName = getLocalName(fullName);
    }

    public static boolean isReferencePattern(String token)
    {
        return token.length() > 2 && token.startsWith("<") && token.endsWith(">");
    }

    public static ReferencePattern make(NLUBuilder builder,String fullName)
    {
        return new ReferencePattern(builder, fullName);
    }

    public static String getReferenceName(String fullName)
    {
        String nameSpec = fullName.substring(0, fullName.length() - 1).substring(1);
        String[] parts = StringUtils.splitByWholeSeparator(nameSpec, COMP_DELIMITER);
        return "<" + parts[0] + ">";
    }

    public static String makeReferenceName(String rawSpeech)
    {
        return "<" +rawSpeech + ">";
    }

    public static String getLocalName(String fullName)
    {
        String nameSpec = fullName.substring(0, fullName.length() - 1).substring(1);
        String[] parts = StringUtils.splitByWholeSeparator(nameSpec, COMP_DELIMITER);
        if (parts.length > 1)
            return parts[1];
        else
            return parts[0];
    }

    @Override
    public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config) {


        Pattern pattern = mBuilder.getConceptReference(mReferenceName);

        ValueConstructor defaultConstructor = mBuilder.getDefaultConfiguration(mReferenceName).getValueConstructor();

        config.setValueConstructor(defaultConstructor);
        if (pattern != null)
            return pattern.getBestMatchingPrefix(inputTokens, config);
        throw new IllegalStateException("Undefined speech function: [" + mReferenceName + "]");
    }
}
