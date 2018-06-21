package com.evolved.automata;

import org.apache.commons.lang3.tuple.Pair;

import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 10/18/16.
 */
public class Metaphone {

    final static HashSet<Character > _vowelSet;

    static {
        _vowelSet = new HashSet()
        {
            {
                add(Character.valueOf('a'));
                add(Character.valueOf('e'));
                add(Character.valueOf('i'));
                add(Character.valueOf('o'));
                add(Character.valueOf('u'));
            }
        };
    }



    String _original;

    StringBuilder _mainVariant;
    StringBuilder _alternateVariant;
    int _lastIndex;
    boolean _hasAlternative = false;
    boolean _isSlavoGermanicP = false;

    public Metaphone(String original)
    {
        _lastIndex = original.length()-1;
        _original = original.toLowerCase() + "     ";
        _mainVariant = new StringBuilder();
        _alternateVariant = new StringBuilder();
        _isSlavoGermanicP = _original.indexOf("w") > -1 || _original.indexOf("k") > -1 || _original.indexOf("cz") > -1 || _original.indexOf("witz") > -1;
    }

    private void appendToMetaphone(String conversion)
    {
        _mainVariant.append(conversion);
        _alternateVariant.append(conversion);
    }

    private void appendToMetaphone(String toMain, String toAlternative)
    {
        _mainVariant.append(toMain);
        if (toAlternative != null)
        {
            _hasAlternative = true;
            if (toAlternative.charAt(0) != ' ')
                _alternateVariant.append(toAlternative);
        }
        else if (toMain.charAt(0) != ' ')
            _alternateVariant.append(toMain);

    }

    private boolean testSubstringAgainst(int start, int length, String ... testStrings)
    {
        if (start < 0)
            return false;

        if (start + length>_original.length())
            return false;

        String substring = _original.substring(start, start + length);

        for (String compString:testStrings)
        {
            if (substring.equals(compString))
                return true;
        }
        return false;
    }

    private boolean hasVowelAtIndex(int index)
    {
        if (index<0 || index >= _original.length())
            return false;
        return _vowelSet.contains(Character.valueOf(_original.charAt(index)));
    }

    public Pair<String, String> getMetaphone()
    {
        int current = 0;
        while (current<=_lastIndex)
        {


            switch (_original.charAt(current))
            {
                case 'a':
                case 'e':
                case 'i':
                case 'o':
                case 'u':
                case 'y':
                    if (current == 0)
                        appendToMetaphone("a");
                    current++;
                    break;
                case 'b':
                    appendToMetaphone("p");
                    if (_original.charAt(current + 1) == 'b')
                        current+=2;
                    else
                        current++;
                    break;
                case 'c':
                    if (current > 1 &&
                            !hasVowelAtIndex(current - 2) &&
                            testSubstringAgainst(current - 1, 3, "ach") &&
                            _original.charAt(current + 2) != 'i' &&
                            _original.charAt(current + 2) != 'e')
                    {
                        appendToMetaphone("k");
                        current += 2;
                        break;
                    }

                    if (current == 0 && testSubstringAgainst(current, 6, "caesar"))
                    {
                        appendToMetaphone("s");
                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current, 2, "ch"))
                    {
                        if (current > 0 && testSubstringAgainst(current, 4, "chae"))
                        {
                            appendToMetaphone("k", "x");
                            current+=2;
                            break;
                        }

                        if (current == 0 &&
                                (
                                        testSubstringAgainst(current + 1, 5, "harac", "haris") ||
                                        testSubstringAgainst(current + 1, 3, "hor", "hym", "hia", "hem")
                                ) &&
                                !testSubstringAgainst(0, 5, "chore"))
                        {
                            appendToMetaphone("k");
                            current+=2;
                            break;
                        }

                        if ((
                                testSubstringAgainst(0, 4, "van", "von") ||
                                testSubstringAgainst(0, 3, "sch") ||
                                testSubstringAgainst(current - 2, 6, "orches", "archit", "orchid") ||
                                testSubstringAgainst(current + 2, 1, "t", "s") ||
                                testSubstringAgainst(current - 1, 1, "a", "e", "o", "u") ||
                                current == 0
                            ) &&
                           testSubstringAgainst(current + 2, 1, "b", "f", "h", "l", "m", "n", "r", "v", "w", " "))
                        {
                            appendToMetaphone("k");
                        }
                        else
                        {
                            if (current > 0)
                            {
                                if (testSubstringAgainst(0, 2, "mc"))
                                {
                                    appendToMetaphone("k");
                                }
                                else
                                {
                                    appendToMetaphone("x", "k");
                                }
                            }
                            else
                            {
                                appendToMetaphone("x");
                            }
                        }
                        current+=2;
                        break;

                    }

                    if (testSubstringAgainst(current, 2, "cz") &&
                            !testSubstringAgainst(current - 2, 4, "wicz"))
                    {
                        appendToMetaphone("s", "x");
                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current + 1, 3, "cia"))
                    {
                        appendToMetaphone("x");
                        current+=3;
                        break;
                    }

                    if (testSubstringAgainst(current, 2, "cc") &&
                            !(current == 1 && _original.charAt(0) == 'm'))
                    {
                        if (testSubstringAgainst(current + 2, 1, "e", "h", "i") &&
                                !testSubstringAgainst(current + 2, 2, "hu"))
                        {
                            if ((current == 1 && _original.charAt(current - 1) == 'a') ||
                                    testSubstringAgainst(current - 1, 5, "uccee", "ucces"))
                            {
                                appendToMetaphone("ks");
                            }
                            else
                            {
                                appendToMetaphone("x");
                            }
                            current+=3;
                            break;
                        }
                        else
                        {
                            appendToMetaphone("k");
                            current+=2;
                            break;
                        }
                    }

                    if (testSubstringAgainst(current, 2, "ck", "cg", "cq"))
                    {
                        appendToMetaphone("k");
                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current, 2, "ci", "ce", "cy"))
                    {
                        if (testSubstringAgainst(current, 3, "cio", "cie", "cia"))
                        {
                            appendToMetaphone("s", "x");
                        }
                        else
                        {
                            appendToMetaphone("s");
                        }
                        current+=2;
                        break;
                    }

                    appendToMetaphone("k");

                    if (testSubstringAgainst(current + 1, 2, " c", " q", " g"))
                    {
                        current+=3;
                    }
                    else if (testSubstringAgainst(current + 1, 1, "c", "k", "q") &&
                            !testSubstringAgainst(current + 1, 2, "ce", "ci"))
                    {
                        current +=2;
                    }
                    else
                    {
                        current++;
                    }
                    break;
                case 'd':
                    if (testSubstringAgainst(current, 2, "dg"))
                    {
                        if (testSubstringAgainst(current + 2, 1, "e", "i", "y"))
                        {
                            appendToMetaphone("j");
                            current+=3;
                            break;
                        }
                        else
                        {
                            appendToMetaphone("tk");
                            current+=2;
                            break;
                        }
                    }

                    if (testSubstringAgainst(current, 2, "dt", "dd"))
                    {
                        appendToMetaphone("t");
                        current+=2;
                        break;
                    }

                    appendToMetaphone("t");
                    current++;
                    break;
                case 'f':
                    if (_original.charAt(current + 1) == 'f')
                    {
                        current+=2;
                    }
                    else
                    {
                        current++;
                    }
                    appendToMetaphone("f");
                    break;
                case 'g':
                    if (_original.charAt(current + 1) == 'h')
                    {
                        if (current > 0 && !hasVowelAtIndex(current - 1))
                        {
                            appendToMetaphone("k");
                            current+=2;
                            break;
                        }

                        if (current < 3)
                        {
                            if (current == 0)
                            {
                                if (_original.charAt(current + 2) == 'i')
                                {
                                    appendToMetaphone("j");
                                }
                                else
                                {
                                    appendToMetaphone("k");
                                }
                                current+=2;
                                break;
                            }
                        }

                        if ((current > 1 && testSubstringAgainst(current - 2, 1, "b", "d", "h")) ||
                                (current > 2 && testSubstringAgainst(current - 3, 1, "b", "d", "h")) ||
                                (current > 3 && testSubstringAgainst(current - 4, 1, "b", "d", "h")))
                        {
                            current+=2;
                            break;
                        }
                        else
                        {
                            if (current > 2 &&
                                    _original.charAt(current - 1) == 'u' &&
                                    testSubstringAgainst(current - 3, 1, "c", "g", "l", "r", "t"))
                            {
                                appendToMetaphone("f");
                            }
                            else if (current > 0 && _original.charAt(current - 1) != 'i')
                            {
                                appendToMetaphone("k");
                            }
                            current+=2;
                            break;
                        }
                    }

                    if (_original.charAt(current + 1) == 'n')
                    {
                        if (current == 1 && hasVowelAtIndex(0) && !_isSlavoGermanicP)
                        {
                            appendToMetaphone("kn", "n");
                        }
                        else
                        {
                            if (!testSubstringAgainst(current + 2, 2, "ey") &&
                                    _original.charAt(current + 1) != 'y'
                                    && !_isSlavoGermanicP)
                            {
                                appendToMetaphone("n", "kn");
                            }
                            else
                            {
                                appendToMetaphone("kn");
                            }
                        }

                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current + 1, 2, "li") && !_isSlavoGermanicP)
                    {
                        appendToMetaphone("kl", "l");
                        current+=2;
                        break;
                    }

                    if (current == 0 &&
                            (_original.charAt(current + 1) == 'y' ||
                            testSubstringAgainst(current + 1, 2, "es", "ep", "eb", "el", "ey", "ib", "il", "in", "ie", "ei", "er")))
                    {
                        appendToMetaphone("k", "j");
                        current+=2;
                        break;
                    }

                    if ((testSubstringAgainst(current + 1, 2, "er") ||
                            _original.charAt(current + 1) == 'y') &&
                            !testSubstringAgainst(0, 6, "danger", "ranger", "manger") &&
                            !testSubstringAgainst(current - 1, 1, "e", "i") &&
                            !testSubstringAgainst(current - 1, 3, "rgy", "ogy"))
                    {
                        appendToMetaphone("k", "j");
                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current + 1, 1, "e", "i", "y") ||
                            testSubstringAgainst(current - 1, 4, "aggi", "oggi"))
                    {
                        if (testSubstringAgainst(0, 4, "van", "von") ||
                                testSubstringAgainst(0, 3, "sch") ||
                                testSubstringAgainst(current + 1, 2, "et"))
                        {
                            appendToMetaphone("k");
                        }
                        else
                        {
                            if (testSubstringAgainst(current + 1, 4, "ier "))
                            {
                                appendToMetaphone("j");
                            }
                            else
                            {
                                appendToMetaphone("j", "k");
                            }
                        }

                        current +=2;
                        break;
                    }

                    if (_original.charAt(current + 1) == 'g')
                        current+=2;
                    else
                        current++;

                    appendToMetaphone("k");
                    break;
                case 'h':
                    if (current == 0 || hasVowelAtIndex(current - 1) && hasVowelAtIndex(current + 1))
                    {
                        appendToMetaphone("h");
                        current+=2;
                    }
                    else
                        current++;
                    break;
                case 'j':
                    if (testSubstringAgainst(current, 4, "jose") ||
                            testSubstringAgainst(0, 4, "san "))
                    {
                        if ((current == 0 && _original.charAt(current + 4) == ' ') ||
                                testSubstringAgainst(0, 4, "san "))
                        {
                            appendToMetaphone("h");
                        }
                        else
                            appendToMetaphone("j", "h");
                        current++;
                        break;
                    }

                    if (current == 0 && !testSubstringAgainst(current, 4, "jose"))
                    {
                        appendToMetaphone("j", "a");
                    }
                    else
                    {
                        if (hasVowelAtIndex(current - 1) &&
                                !_isSlavoGermanicP &&
                                (_original.charAt(current + 1) == 'a' || _original.charAt(current + 1) == 'o'))
                        {
                            appendToMetaphone("j", "h");
                        }
                        else
                        {
                            if (current == _lastIndex)
                                appendToMetaphone("j", " ");
                            else if (!testSubstringAgainst(current + 1, 1, "l", "t", "k", "s", "n", "m", "b", "z") &&
                                    !testSubstringAgainst(current - 1, 1, "s", "k", "l"))
                            {
                                appendToMetaphone("j");
                            }
                        }
                    }

                    if (_original.charAt(current + 1) == 'j')
                        current+=2;
                    else
                        current++;
                    break;
                case 'k':
                    if (_original.charAt(current + 1) == 'k')
                        current+=2;
                    else
                        current++;
                    appendToMetaphone("k");
                    break;
                case 'l':
                    if (_original.charAt(current + 1) == 'l')
                    {
                        if ((current == _lastIndex - 2) &&
                                testSubstringAgainst(current - 1, 4, "illo", "illa", "alle") ||
                                (testSubstringAgainst(_lastIndex - 1, 2, "as", "os") ||
                                testSubstringAgainst(_lastIndex, 1, "a", "o")) &&
                                testSubstringAgainst(current - 1, 4, "alle"))
                        {
                            appendToMetaphone("l");
                            current+=2;
                            break;
                        }
                        current +=2;
                    }
                    else
                        current++;
                    appendToMetaphone("l");
                    break;
                case 'm':
                    if (testSubstringAgainst(current - 1, 3, "umb") &&
                            (current + 1 == _lastIndex || testSubstringAgainst(current + 2, 2, "er")) ||
                            _original.charAt(current + 1) == 'm')
                    {
                        current+=2;
                    }
                    else
                        current++;
                    appendToMetaphone("m");
                    break;
                case 'n':
                    if (_original.charAt(current + 1) == 'n')
                        current+=2;
                    else
                        current++;
                    appendToMetaphone("n");
                    break;
                case 'p':
                    if (_original.charAt(current + 1) == 'h')
                    {
                        appendToMetaphone("f");
                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current + 1, 1, "b", "p"))
                        current +=2;
                    else
                    {
                        current++;
                        appendToMetaphone("p");
                    }
                    break;
                case 'q':
                    if (_original.charAt(current + 1) == 'q')
                        current+=2;
                    else
                    {
                        current++;

                    }
                    appendToMetaphone("k");
                    break;
                case 'r':
                    if (current == _lastIndex &&
                            !_isSlavoGermanicP &&
                            testSubstringAgainst(current - 2, 2, "ie") &&
                            !testSubstringAgainst(current - 4, 2, "me", "ma"))
                    {
                        appendToMetaphone("", "r");
                    }
                    else
                        appendToMetaphone("r");

                    if (_original.charAt(current + 1) == 'r')
                        current+=2;
                    else
                        current++;
                    break;
                case 's':
                    if (testSubstringAgainst(current - 1, 3, "isl", "ysl"))
                    {
                        current++;
                        break;
                    }

                    if (current == 0 &&
                            testSubstringAgainst(current, 5, "sugar"))
                    {
                        appendToMetaphone("x", "s");
                        current++;
                        break;
                    }

                    if (testSubstringAgainst(current, 2, "sh"))
                    {
                        if (testSubstringAgainst(current + 1, 4, "heim", "hoek", "holm", "holz"))
                            appendToMetaphone("s");
                        else
                            appendToMetaphone("x");
                        current+=2;
                        break;
                    }

                    if (testSubstringAgainst(current, 3, "sio", "sia") ||
                            testSubstringAgainst(current, 4, "sian"))
                    {
                        if (!_isSlavoGermanicP)
                            appendToMetaphone("s", "x");
                        else
                            appendToMetaphone("s");
                        current+=3;
                        break;
                    }

                    if ((current == 0 &&
                            testSubstringAgainst(current + 1, 1, "m", "n", "l", "w") ||
                            testSubstringAgainst(current + 1, 1, "z")))
                    {
                        appendToMetaphone("s", "x");
                        if (testSubstringAgainst(current + 1, 1, "z"))
                            current+=2;
                        else
                            current++;
                        break;
                    }

                    if (testSubstringAgainst(current, 2, "sc"))
                    {
                        if (_original.charAt(current + 2) == 'h')
                        {
                            if (testSubstringAgainst(current + 3, 2, "oo", "er", "en", "uy", "ed", "em"))
                            {
                                if (testSubstringAgainst(current + 3, 2, "er", "en"))
                                    appendToMetaphone("x", "sk");
                                else
                                    appendToMetaphone("sk");
                                current+=3;
                                break;
                            }
                            else
                            {
                                if (current == 0 && !hasVowelAtIndex(3) && _original.charAt(3) != 'w')
                                    appendToMetaphone("x", "s");
                                else
                                    appendToMetaphone("x");
                                current+=3;
                                break;
                            }
                        }

                        if (testSubstringAgainst(current + 2, 1, "e", "i", "y"))
                        {
                            appendToMetaphone("s");
                            current+=3;
                            break;
                        }

                        appendToMetaphone("sk");
                        current+=3;
                        break;
                    }

                    if (current == _lastIndex &&
                            testSubstringAgainst(current - 2, 2, "ai", "oi"))
                        appendToMetaphone("", "s");
                    else
                        appendToMetaphone("s");

                    if (testSubstringAgainst(current + 1, 1, "s", "z"))
                        current += 2;
                    else
                        current++;
                    break;
                case 't':
                    if (testSubstringAgainst(current, 4, "tion"))
                    {
                        appendToMetaphone("x");
                        current+=3;
                        break;
                    }

                    if (testSubstringAgainst(current, 3, "tia", "tch"))
                    {
                        appendToMetaphone("x");
                        current+=3;
                        break;
                    }

                    if (testSubstringAgainst(current, 2, "th") ||
                            testSubstringAgainst(current, 3, "tth"))
                    {
                        if (testSubstringAgainst(current + 2, 2, "om", "am") ||
                                testSubstringAgainst(0, 4, "van ", "von ") ||
                                testSubstringAgainst(0, 3, "sch"))
                            appendToMetaphone("t");
                        else
                            appendToMetaphone("0", "t");
                        current+=2;
                        break;

                    }

                    if (testSubstringAgainst(current + 1, 1, "t", "d"))
                        current+=2;
                    else
                        current++;
                    appendToMetaphone("t");
                    break;
                case 'v':
                    if (_original.charAt(current + 1) == 'v')
                        current+=2;
                    else
                        current++;
                    appendToMetaphone("f");
                    break;
                case 'w':
                    if (testSubstringAgainst(current, 2, "wr"))
                    {
                        appendToMetaphone("r");
                        current+=2;
                        break;
                    }

                    if (current == 0 &&
                            hasVowelAtIndex(current + 1) ||
                            testSubstringAgainst(current, 2, "wh"))
                    {
                        if (hasVowelAtIndex(current + 1))
                            appendToMetaphone("a", "f");
                        else
                            appendToMetaphone("a");
                    }

                    if ((current == _lastIndex &&
                        hasVowelAtIndex(current - 1)) ||
                            testSubstringAgainst(current - 1, 5, "ewski", "ewsky", "owski", "owsky") ||
                            testSubstringAgainst(0, 3, "sch"))
                    {
                        appendToMetaphone("", "f");
                        current++;
                        break;
                    }

                    if (testSubstringAgainst(current, 4, "wicz", "witz"))
                    {
                        appendToMetaphone("ts", "fx");
                        current+=4;
                        break;

                    }

                    current++;
                    break;
                case 'x':
                    if (!(current == _lastIndex &&
                         testSubstringAgainst(current - 3, 3, "iau", "eau") ||
                        testSubstringAgainst(current - 2, 2, "au", "ou")))
                    {
                        appendToMetaphone("ks");
                    }

                    if (testSubstringAgainst(current + 1, 1, "c", "x"))
                        current+=2;
                    else
                        current++;
                    break;
                case 'z':
                    if (_original.charAt(current + 1) == 'h')
                    {
                        appendToMetaphone("j");
                        current+=2;
                        break;
                    }
                    else
                    {
                        if (testSubstringAgainst(current + 1, 2, "zo", "zi", "za") ||
                                (_isSlavoGermanicP && current > 0 && _original.charAt(current - 1) != 't'))
                        {
                            appendToMetaphone("s", "ts");
                        } else
                            appendToMetaphone("s");
                    }

                    if (_original.charAt(current + 1) == 'z')
                        current+=2;
                    else
                        current++;
                    break;
                default:
                    current++;
            }

        }


        return Pair.of(_mainVariant.toString(), _alternateVariant.toString());


    }


}
