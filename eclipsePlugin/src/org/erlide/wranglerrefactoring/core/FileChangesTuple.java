package org.erlide.wranglerrefactoring.core;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import org.erlide.wranglerrefactoring.util.TextFileDiffTool;

public class FileChangesTuple {

	String oldPath;
	String newPath;
	String newFileContent;

	public FileChangesTuple(String oldPath, String newPath,
			String newFileContent) {
		this.oldPath = oldPath;
		this.newPath = newPath;
		this.newFileContent = newFileContent;
	}

	public Change createChanges() throws IOException, CoreException {
		IFile eclipseRep = findEclipseRepresentation(oldPath);

		TextFileChange change = new TextFileChange(newPath, eclipseRep);
		File tf = new File(oldPath);
		ArrayList<TextEdit> edits = TextFileDiffTool._createEdits(tf,
				newFileContent);
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

	private IFile findEclipseRepresentation(String oldPath)
			throws CoreException, IOException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		Path p = new Path(oldPath);
		IFile[] files = root.findFilesForLocation(p);
		if (files.equals(null) || files.length != 1)
			throw new IOException("File not found");

		return files[0];

		/*
		 * for (IResource actRes : root.members()) { if (actRes.getType() ==
		 * IResource.FILE && actRes.getLocation().toOSString() == oldPath) {
		 * return (IFile) actRes; } }
		 * 
		 * return null;
		 */

	}
}
