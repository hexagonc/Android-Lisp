package com.evolved.automata.lisp.speech;

import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.Buffer;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 6/25/17.
 */

public class WordMetadata {
    public enum VerbTense
    {
        INFINITIVE,
        PRESENT_PARTICIPLE,
        SIMPLE_PAST,
        PAST_PARTICIPLE,
        THIRD_PERSON_SINGULAR
    }

    public enum ConjugationInfoType
    {
        POINTER, CONJUGATIONS
    }

    static abstract class ConjugationInfo
    {
        ConjugationInfoType _type;
        protected ConjugationInfo(ConjugationInfoType type)
        {
            _type = type;
        }

        public ConjugationInfoType getType()
        {
            return _type;
        }

        public static ConjugationInfo from(String infinitive, VerbTense tense)
        {
            return new ConjugationPointer(infinitive, tense);
        }

        public static ConjugationInfo from(HashMap<VerbTense, String> conjMap)
        {
            return new VerbConjugations(conjMap);
        }
    }

    static class ConjugationPointer extends ConjugationInfo
    {
        String _infinitive;
        VerbTense _tense;

        ConjugationPointer(String infinitive, VerbTense tense)
        {
            super(ConjugationInfoType.POINTER);
            _tense = tense;
            _infinitive = infinitive;
        }

        public VerbTense getTense()
        {
            return _tense;
        }

        public String getInfinitive()
        {
            return _infinitive;
        }
    }

    static class VerbConjugations extends ConjugationInfo
    {
        HashMap<VerbTense, String> _conjugationMap;

        VerbConjugations(HashMap<VerbTense, String> conjugations)
        {
            super(ConjugationInfoType.CONJUGATIONS);
            _conjugationMap = conjugations;
        }

        public HashMap<VerbTense, String> getConjugations()
        {
            return _conjugationMap;
        }
    }


    public static HashMap<String, LinkedList<ConjugationInfo >> mVerbConjugationIndex = new HashMap<String, LinkedList<ConjugationInfo >>();


    public static void addVerbConjugations(String infinitive, String simplePast, String pastParticiple, String presentParticiple, String thirdPerson)
    {
        HashMap<VerbTense, String> conjMap = new HashMap<VerbTense, String>(5);
        conjMap.put(VerbTense.INFINITIVE, infinitive);
        conjMap.put(VerbTense.SIMPLE_PAST, simplePast);
        conjMap.put(VerbTense.PAST_PARTICIPLE, pastParticiple);
        conjMap.put(VerbTense.PRESENT_PARTICIPLE, presentParticiple);
        conjMap.put(VerbTense.THIRD_PERSON_SINGULAR, thirdPerson);


        LinkedList<ConjugationInfo> prior = mVerbConjugationIndex.get(infinitive);

        if (prior == null)
        {
            mVerbConjugationIndex.put(infinitive, prior = new LinkedList<ConjugationInfo>());
        }
        prior.add(ConjugationInfo.from(conjMap));


        prior = mVerbConjugationIndex.get(simplePast);

        if (prior == null)
        {
            mVerbConjugationIndex.put(simplePast, prior = new LinkedList<ConjugationInfo>());
        }
        prior.add(ConjugationInfo.from(infinitive, VerbTense.SIMPLE_PAST));

        prior = mVerbConjugationIndex.get(pastParticiple);

        if (prior == null)
        {
            mVerbConjugationIndex.put(pastParticiple, prior = new LinkedList<ConjugationInfo>());
        }
        prior.add(ConjugationInfo.from(infinitive, VerbTense.PAST_PARTICIPLE));


        prior = mVerbConjugationIndex.get(presentParticiple);

        if (prior == null)
        {
            mVerbConjugationIndex.put(presentParticiple, prior = new LinkedList<ConjugationInfo>());
        }
        prior.add(ConjugationInfo.from(infinitive, VerbTense.PRESENT_PARTICIPLE));


        prior = mVerbConjugationIndex.get(thirdPerson);

        if (prior == null)
        {
            mVerbConjugationIndex.put(thirdPerson, prior = new LinkedList<ConjugationInfo>());
        }
        prior.add(ConjugationInfo.from(infinitive, VerbTense.THIRD_PERSON_SINGULAR));

    }

    public static void loadDefaultVerbConjugations() throws IOException
    {
        InputStreamReader reader = null;
        BufferedReader breader =  null;
        try
        {

            ClassLoader loader = WordMetadata.class.getClassLoader();
            InputStream istream = loader.getResourceAsStream("com/evolved/automata/lisp/speech/naiveVerbConjugations.csv");

            reader = new InputStreamReader(istream);
            breader =  new BufferedReader(reader);
            String lineinput;
            LinkedList<String> list = new LinkedList<String>();

            while ((lineinput=breader.readLine())!=null)
            {
                if (lineinput.trim().length()>0)
                {
                    String[] conjugations = StringUtils.split(lineinput, ',');
                    addVerbConjugations(conjugations[0].trim(), conjugations[1].trim(), conjugations[2].trim(), conjugations[3].trim(), conjugations[4].trim());
                }
            }


        }
        finally
        {
            if (null!=breader)
            {
                try
                {
                    breader.close();
                }
                catch (Exception e)
                {

                }
            }
        }
    }

    public static LinkedList<VerbTense> getVerbTense(String verb)
    {
        LinkedList<ConjugationInfo> infoList = mVerbConjugationIndex.get(verb);
        LinkedList<VerbTense> out = new LinkedList<VerbTense>();
        if (infoList != null)
        {
            for (ConjugationInfo info:infoList)
            {
                if (info.getType() == ConjugationInfoType.POINTER)
                {
                    out.add(((ConjugationPointer)info).getTense());
                }
                else
                    out.add(VerbTense.INFINITIVE);
            }
            return out;
        }
        else
            return new LinkedList<VerbTense>();
    }

    // TODO: handling ambiguous verbs
    public static HashMap<VerbTense, String> getConjugations(String verb)
    {
        LinkedList<ConjugationInfo> infoList = mVerbConjugationIndex.get(verb);

        LinkedList<String> infinitiveList = new LinkedList<String>();
        for (ConjugationInfo info:infoList)
        {

            if (info.getType() == ConjugationInfoType.POINTER)
            {
                infinitiveList.add(((ConjugationPointer)info).getInfinitive());
            }
            else
                return ((VerbConjugations)info).getConjugations();
        }

        if (infinitiveList.size() > 0)
            return getConjugations(infinitiveList.getFirst());
        return null;
    }




}
