/**
 * @author György Orosz
 */
package org.erlide.wranglerrefactoring.core;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;

public class RefactoringParameters {

	private ITextEditor editor;
	private IFile file;
	private ITextSelection selection;
	private Integer line;
	private Integer coloumn;
	private IDocument doc;

	public RefactoringParameters() {
	}

	public void init() {
		// TODO: more elegant solution to get the file
		IEditorPart editorPart = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		// TODO: do I need class checking?
		editor = (ITextEditor) editorPart;
		// TODO: do I need class checking?
		IFileEditorInput input = (IFileEditorInput) (editor.getEditorInput());
		file = input.getFile();
		doc = editor.getDocumentProvider().getDocument(input);
		// TODO: do I need class checking?
		selection = (ITextSelection) (editor.getSelectionProvider()
				.getSelection());
		line = selection.getStartLine() + 1;
		coloumn = calculateColoumnFromOffset(selection.getOffset());
	}

	private int calculateColoumnFromOffset(int offset) {
		int sumpos = 0;
		int i = 0;

		try {
			while (sumpos + doc.getLineLength(i) - 1 < offset) {
				sumpos += doc.getLineLength(i);
				++i;
			}
		} catch (BadLocationException e) {
			// TODO: what the hell do I need here???
			e.printStackTrace();
		}

		return offset - sumpos + 1;
	}

	public Integer getLine() {
		return line;
	}

	public Integer getColoumn() {
		return coloumn;
	}

	public IFile getFile() {
		return file;
	}

}
