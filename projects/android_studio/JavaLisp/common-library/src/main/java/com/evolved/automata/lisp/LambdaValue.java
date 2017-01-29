package com.evolved.automata.lisp;

public class LambdaValue extends Value {

	FunctionTemplate _lambda;
	public LambdaValue(FunctionTemplate lambda)
	{
		_lambda = lambda;
		_type = Type.LAMBDA;
	}
	
	@Override
	public FunctionTemplate getLambda()
	{
		return _lambda;
	}
	
	
	@Override
	public boolean equals(Value v) {
		return v!=null && v.isLambda() &&  (_lambda == v.getLambda() ||
				serializedForm(true).equals(((LambdaValue)v).serializedForm(true)));
	}

	@Override
	public String toString() {
		
		return addQualifiers(_lambda.toString());
	}

	@Override
	public Value clone() {
		
		//if (_lambda.getName() == null || _lambda.getName().trim().length()==0)
			return new LambdaValue(_lambda);
		//else
		//	return Environment.parse(serializedForm(), true).getFirst();
	}

	@Override
	public String serializedForm()
	{
		return addQualifiers(_lambda.serialize());
	}
	
	
	public String serializedForm(boolean serializeLambdasAsObjects)
	{
		if (!serializeLambdasAsObjects)
			return serializedForm();
		if (_lambda.hasNameP())
			return serializedForm();
		else
			return addQualifiers(((Lambda)_lambda).serializeAsObject());
	}
	
}
