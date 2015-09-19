package com.evolved.automata.parser.general;

public interface CustomTerminalMatcher {
	public boolean match(GeneralizedCharacter gchar);
	public GeneralizedCharacter sample();
}
