package com.evolved.automata.android;
import android.content.*;

public interface ActivityResultHandler {
	public boolean onActivityResult(int requestCode, int resultCode, Intent data);

}
