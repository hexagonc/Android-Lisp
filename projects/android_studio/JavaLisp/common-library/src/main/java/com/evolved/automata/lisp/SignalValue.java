package com.evolved.automata.lisp;

public class SignalValue extends Value 
{

	public SignalValue(Value name, Value value, boolean out)
	{
		_signalValue = (value == null)?Environment.getNull():value;
		_signalName = name;
		if (out)
			_attrib = ExitAttribute.SIGNAL_OUT;
		else
			_attrib = ExitAttribute.SIGNAL;
	}
	
	
	
	@Override
	public boolean equals(Value v) {
		
		return (v.isSignal() && isSignal() || v.isSignalOut() && isSignalOut()) && v.getSignalValue().equals(getSignalValue()) && v.getSignalName().equals(getSignalName());
	}

	@Override
	public String toString() {
		
		if (isSignal())
			return "(signal " + _signalName + " " + _signalValue + ")";
		else
			return "(signal-out " + _signalName + " " + _signalValue + ")";
	}

	@Override
	public Value clone() {
		SignalValue sv = new SignalValue(_signalName, _signalValue, _attrib == ExitAttribute.SIGNAL_OUT);
		return sv;
	}

}
