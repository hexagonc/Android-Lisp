package com.evolved.automata.lisp;

import java.util.UUID;
import java.util.concurrent.CountDownLatch;

public class AsyncFor extends FunctionTemplate{
	

		
		final int EVALUATING_LIST = 0;
		final int BINDING_VARIABLES = 1;
		final int EVALUATING_EXP = 2;
		final int RETURNING_RESULT = 3;
		int _currentState = EVALUATING_LIST;
		long _loopIndex = 0;
		long _maxIndex = 0;
		String _bindingLoopVariableName = null;
		String _bindingLoopIndexName = null;
		Value[] _loopList = null;
		boolean _isListIteration = true;
		Environment[] _innerEnv = null;
		Value[] _innerEnvArgs = null;
		Environment _listInnerEnv =null;
		Thread[] _asyncEvaluators = null;
		CountDownLatch _syncLatch = null; 
		Value _returnValue = null;
		int _minReturnIndex = -1;
		ThreadGroup _asyncEvaluatorThreadGroup;
		public void resetFunctionTemplate()
		{
			_asyncEvaluatorThreadGroup = null;
			_returnValue = null;
			_minReturnIndex = -1;
			_currentState = EVALUATING_LIST;
			_lastFunctionReturn = null;
			_instructionPointer = 0;
			_loopIndex = 0;
			_bindingLoopVariableName = null;
			_bindingLoopIndexName = null;
			_loopList = null;
			_maxIndex = 0;
			_isListIteration = true;
			_innerEnv = null;
			_listInnerEnv =null;
			_asyncEvaluators = null;
		}
		
		public AsyncFor()
		{
			_name = "async-for";	
		}
		
		
		@Override
		public Value evaluate(Environment env, boolean resume)
				throws InstantiationException, IllegalAccessException {
			checkActualArguments(1, true, true);
			
			if (!resume)
				resetFunctionTemplate();
			
			Value result = Environment.getNull();
			while (true)
			{
				switch (_currentState)
				{
					case EVALUATING_LIST:
						if (_listInnerEnv == null)
							_listInnerEnv = new Environment(env);
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_listInnerEnv, resume);
						else
							result = _lastFunctionReturn = _listInnerEnv.evaluate(_actualParameters[1], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if (result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
						if (result.isBreak())
							return resetReturn(result.setBreak(false));
						
						if (result.isList())
						{
							_loopList = result.getList();
							_isListIteration = true;
							_maxIndex = _loopList.length;
							_syncLatch = new CountDownLatch((int)_maxIndex);
							_innerEnv = new Environment[(int)_maxIndex];
							_asyncEvaluators = new Thread[(int)_maxIndex];
							_innerEnvArgs = new Value[(int)_maxIndex];
							_asyncEvaluatorThreadGroup = new ThreadGroup(UUID.randomUUID().toString());
						}
						else if (NLispTools.isNumericType(result) && result.getIntValue()>=0)
						{
							_maxIndex = result.getIntValue();
							_isListIteration = false;
							_syncLatch = new CountDownLatch((int)_maxIndex);
							_innerEnv = new Environment[(int)_maxIndex];
							_asyncEvaluators = new Thread[(int)_maxIndex];
							_innerEnvArgs = new Value[(int)_maxIndex];
							_asyncEvaluatorThreadGroup = new ThreadGroup(UUID.randomUUID().toString());
						}
						else 
							throw new RuntimeException("Second argument to 'for' must be a list or a non-negative number");
						 _currentState = BINDING_VARIABLES;
						 _loopIndex = 0;
						 break;
					case RETURNING_RESULT:
						if (_returnValue != null)
							return resetReturn(_returnValue);
						
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_listInnerEnv, resume);
						else
							result = _lastFunctionReturn = _listInnerEnv.evaluate(_actualParameters[2], false);
						if (result.isContinuation())
							return continuationReturn(result);
						if (result.isBreak())
							return resetReturn(result.setBreak(false));
						else
							return resetReturn(result);
					case BINDING_VARIABLES:
						
						if (_isListIteration)
						{
							
							if (_actualParameters[0].isList())
							{
								if (_bindingLoopVariableName == null)
									_bindingLoopVariableName = _actualParameters[0].getList()[0].getString();
								
								
								if (_actualParameters[0].getList().length>1)
								{
									if (_bindingLoopIndexName == null)
										_bindingLoopIndexName = _actualParameters[0].getList()[1].getString();
								}
							}
							else
							{
								_bindingLoopVariableName = _actualParameters[0].getString();
							}
							for (int i = 0;i<_maxIndex;i++)
							{
								setupEnvironment(i);
							}
							
						}
						else
						{
							_bindingLoopVariableName = _actualParameters[0].getString();
							for (int i = 0;i<_maxIndex;i++)
							{
								setupEnvironment(i, true);
							}
						}
						
						_currentState = EVALUATING_EXP;
						break;
					case EVALUATING_EXP:
						for (int i = 0;i<_maxIndex;i++)
						{
							_asyncEvaluators[i].start();
						}
						try
						{
							_syncLatch.await();
						}
						catch (InterruptedException ie)
						{
							
						}
						_currentState = RETURNING_RESULT;
						break;
				}
			}
			
			
		}
		
		private void setupEnvironment(final int index, boolean bindNumbers)
		{
			_innerEnv[index] = new Environment(_listInnerEnv);
			if (bindNumbers)
				_innerEnv[index].mapValue(_bindingLoopVariableName, NLispTools.makeValue(index));
			else
				_innerEnv[index].mapValue(_bindingLoopVariableName, _loopList[(int)index].clone());
			if (_bindingLoopIndexName != null){
				_innerEnv[index].mapValue(_bindingLoopIndexName, NLispTools.makeValue(index));
			}
			final Value loopArg = _actualParameters[3].clone();
			_asyncEvaluators[index] = new Thread(_asyncEvaluatorThreadGroup, "async-for-" + _loopList[(int)index].toString()){
				public void run()
				{
					Value result = null;
					try
					{
						result = _innerEnv[index].evaluate(loopArg, false);
						
						
						if (result.isReturn() || result.isSignal() || result.isSignalOut())
						{
							if (_returnValue == null || index < _minReturnIndex)
							{
								_minReturnIndex = index;
								_returnValue = result;
							}
						}
						if (result.isBreak())
						{
							result.setBreak(false);
							if (_returnValue == null || index < _minReturnIndex)
							{
								_minReturnIndex = index;
								_returnValue = result;
							}
						}
					}
					catch (Exception e)
					{
						e.printStackTrace();
						_asyncEvaluatorThreadGroup.interrupt();
					}
					finally
					{
						_syncLatch.countDown();
					}
				}
			};
			
			
		}
		
		private void setupEnvironment(int index)
		{
			setupEnvironment(index, false);
		}
		
		

}
