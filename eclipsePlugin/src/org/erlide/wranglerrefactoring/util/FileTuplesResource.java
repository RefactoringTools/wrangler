package org.erlide.wranglerrefactoring.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;

//constructor()->createCopy()->createhanges()->dispose()
public class FileTuplesResource {

	private File tmpFolder;
	private File oldFile;
	private File newFile;
	private IFile eclipseFile;

	public FileTuplesResource(IFile eclipseFile, File folder) {
		this.tmpFolder = folder;
		this.eclipseFile = eclipseFile;
		this.oldFile = eclipseFile.getLocation().toFile();
	}

	public Change createChanges() throws IOException {
		TextFileChange change = new TextFileChange(oldFile.getName(),
				eclipseFile);

		ArrayList<TextEdit> edits = TextFileDiffTool.createEdits(oldFile,
				newFile);
		MultiTextEdit multiEdit = new MultiTextEdit();
		if (!edits.isEmpty()) {
			for (TextEdit edit : edits) {
				multiEdit.addChild(edit);
			}
			change.setEdit(multiEdit);
			return change;
		} else
			return null;
	}

	public IFile getEclipseFile() {
		return eclipseFile;
	}

	public File getFileCopy() {
		return newFile;
	}

	public File createCopy() throws IOException {
		newFile = copy();
		return newFile;
	}

	private File copy() throws IOException {
		// TODO: test if the file is in the right place
		File tmpFile = new File(tmpFolder.getAbsolutePath() + File.separator
				+ oldFile.getName());

		tmpFile.createNewFile();

		BufferedInputStream bis = new BufferedInputStream(new FileInputStream(
				oldFile));

		BufferedOutputStream bos = new BufferedOutputStream(
				new FileOutputStream(tmpFile));

		byte inArr[] = new byte[bis.available()];

		int size = bis.read(inArr, 0, inArr.length);

		bos.write(inArr, 0, size);

		bis.close();

		bos.close();

		return tmpFile;
	}

	public void dispose() {
		if (newFile != null)
			newFile.delete();
		newFile = null;
	}

}
