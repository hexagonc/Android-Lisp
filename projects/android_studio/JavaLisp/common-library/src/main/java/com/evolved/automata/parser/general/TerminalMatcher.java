package com.evolved.automata.parser.general;

import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;



public class TerminalMatcher extends Matcher 
{
	
	String[] _lambdaNames;
	boolean[] _negated;
	boolean _stringTerminalP = false;
	boolean _forcematchP = false;
	boolean _lambdaTerminalP = false;
	String _compValue = null;
	
	public TerminalMatcher(String grammarComponent)
	{
		super(grammarComponent);
		
		_stringTerminalP = grammarComponent.startsWith(Parser.stringCharLiteralStart) && grammarComponent.endsWith(Parser.stringCharLiteralEnd) && grammarComponent.length()>2;
		if (_stringTerminalP)
			_compValue = grammarComponent.substring(1, grammarComponent.length()-1);
		else
			_compValue = Parser.isTerminal(grammarComponent);
		_isTerminal = true;
	}
	
	public TerminalMatcher(String grammarComponent, String[] lambdaNames)
	{
		super(grammarComponent);
		_stringTerminalP = false;
		_lambdaNames = new String[lambdaNames.length];
		_negated = new boolean[lambdaNames.length];
		_lambdaTerminalP = true;
		String conv;
		for (int i=0;i<lambdaNames.length;i++)
		{
			if ((conv = negatedLam(lambdaNames[i]))!=null)
			{
				_lambdaNames[i] = conv;
				_negated[i] = true;
			}
			else
			{
				_lambdaNames[i] = lambdaNames[i];
				_negated[i] = false;
			}
		}
		_isTerminal = true;
	}
	
	String negatedLam(String base)
	{
		if (base.endsWith(Parser.negation))
			return base.substring(0,base.length()-1);
		else
			return null;
	}
	
	public TerminalMatcher(String grammarComponent, String[] lambdaNames, boolean[] negated,  PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		super(global, local, grammarComponent);
		_lambdaNames = lambdaNames;
		_negated = negated;
		_isTerminal = true;
		_lambdaTerminalP = true;
	}
	
	public Matcher clone(PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		TerminalMatcher matcher = new TerminalMatcher(_grammarComponent, _lambdaNames, _negated, global, local);
		matcher._stringTerminalP =  _stringTerminalP;
		matcher._templateIndexInParent = _templateIndexInParent;
		matcher._templateParent = _templateParent;
		matcher.setTag(_tag);
		matcher._lambdaTerminalP = _lambdaTerminalP;
		matcher._compValue = _compValue;
		matcher._grammarComponent = _grammarComponent;
		return matcher;
	}
	
	
	
	public LinkedList<Matcher> match()
	{
		Matcher parent = null;
		if (_templateParent!=null || _local == null)
		{
			if (_templateParent != null && _local == null)
				parent = _templateParent.clone(_global, null);
			if (_local == null)
			{
				_local = new PatternParser.LocalState(parent, _templateIndexInParent, 0);
			}
		}
			
		
		int startIndex = _local._startIndex;
		Sequence input = _global._buffer;
		GeneralizedCharacter gchar = null;
		if (input.size()<=startIndex || (gchar = input.get(startIndex))._isStartCharacter  )
			throw new PatternParser.EndOfStreamException(this);
		if (!_forcematchP && !gchar.isSkipCharacter())
		{
			
			if (!_lambdaTerminalP && (gchar instanceof TextCharacter))
			{
				String cvalue = ((TextCharacter)gchar).getTextValue();
				if (_stringTerminalP) // terminal is an entire string
				{
					if (!cvalue.equals(_compValue))
					{
						if (_local._parent!=null)
							return _local._parent.updateFromParseFailure(_local._indexInParent);
						else
							return null;
					}
					
				}
				else
				{
					if (!matchTerminal(_compValue, cvalue.charAt(0)))
					{
						
						if (_local._parent!=null)
							return _local._parent.updateFromParseFailure(_local._indexInParent);
						else
						{
							return null;
						}
					}
				}
				
			}
			else
			{
				String fName;
				boolean result = false;
				CustomTerminalMatcher cm = null;
				for (int i=0;i<_lambdaNames.length;i++)
				{
					fName = _lambdaNames[i];
					
					if ((cm = _global.getCustomTerminalMatcher(fName))!=null)
					{
						result = cm.match(gchar);
					}
					
					if (!result&&!_negated[i] || result&&_negated[i])
					{
						
						if (_local._parent!=null)
							return _local._parent.updateFromParseFailure(_local._indexInParent);
						else
							return null;
					}
				}
			}
		}
		
		
		LinkedList<Matcher> oList = new LinkedList<Matcher>();
		
		_finalized = true;
		_endIndex = startIndex+1;
		
		if (_tag!=null)
		{
			gchar.addTag(this, _tag);
		}
		if (_local._parent!=null)
			return _local._parent.updateFromParseSuccess(_local._indexInParent, _endIndex);
		else
		{
			oList.add(this);
			finalize();
			return oList;
		}
	}
	
	
	public void forceNextMatch()
	{
		_forcematchP = true;
	}
	
	public GeneralizedCharacter sample()
	{
		if (_stringTerminalP)
		{
			return new TextCharacter(_grammarComponent.substring(1, _grammarComponent.length() - 1), false);
		}
		else if (_lambdaNames!=null && _lambdaNames.length>0)
		{
			CustomTerminalMatcher cm = _global.getCustomTerminalMatcher(_lambdaNames[0]);
			if (cm!=null)
				return cm.sample();
			
		}
		else if (_grammarComponent.equals("#"))
		{
			
			return new TextCharacter("" + randomInt(10), true);
		}
		else if (_grammarComponent.equals("@"))
		{
			if (randomInt(2)>0)
			{
				int offset = randomInt(27)+65;
				return new TextCharacter( new String(new int[]{offset}, 0,1), true);
			}
			else
			{
				int offset = randomInt(27)+97;
				return new TextCharacter( new String(new int[]{offset}, 0,1), true);
			}
		}
		else if (_grammarComponent.equals("$"))
		{
			return new TextCharacter( " ", true);
		}
		else if (_grammarComponent.equals("~"))
		{
			int offset = randomInt(95)+32;
			return new TextCharacter( new String(new int[]{offset}, 0,1), true);
		}
		return null;
	}
	
	private int randomInt(int upperbound)
	{
		double d = upperbound * 1000*Math.random();
		return (int)(d/1000);
	}
	
	public boolean quickMatch(GeneralizedCharacter gchar)
	{
		if (_stringTerminalP && (gchar instanceof TextCharacter))
		{
			String cvalue = ((TextCharacter)gchar).getTextValue();
			if (((TextCharacter)gchar).isSingleCharacter())
			{
				if (!matchTerminal(_compValue, cvalue.charAt(0)))
				{
					return false;
				}
			}
			else
			{
				if (!cvalue.equals(_compValue))
				{
					return false;
				}
			}
			
		}
		else
		{
			String fName;
			boolean rvalue;
			for (int i=0;i<_lambdaNames.length;i++)
			{
				fName = _lambdaNames[i];
				CustomTerminalMatcher cm;
				if ((cm = _global.getCustomTerminalMatcher(fName))!=null)
				{
					rvalue =  cm.match(gchar);
					if (rvalue&&!_negated[i] || !rvalue&&_negated[i])
					{
						return false;
					}
					return rvalue;
				}
				
				
			}
		}
		return true;
	}
	
	
	private boolean matchTerminal(String terminal, char raw)
	{
		if (terminal.length()>1)
			return terminal.charAt(1)==raw;
		else
		{
			
			if (terminal.substring(0,1).equals(Parser.wildcard))
				return true;
			else if (terminal.substring(0,1).equals(Parser.whitespace))
				return Character.isWhitespace(raw);
			else if (terminal.substring(0,1).equals(Parser.numeric))
				return Character.isDigit(raw);
			else if (terminal.substring(0,1).equals(Parser.letter))
				return Character.isLetter(raw);
			else
				return terminal.charAt(0) == raw;
		}
	}
	
	
}
