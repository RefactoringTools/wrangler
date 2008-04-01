/**
 * @author György Orosz
 */
package org.erlide.wranglerrefactoring.core;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

public class RefactoringParameters {

	private ITextEditor editor;
	private IFile file;
	private ITextSelection selection;
	private Integer startLine;
	private Integer startColoumn;
	private Integer endLine;
	private Integer endColoumn;
	private IDocument doc;

	public RefactoringParameters() {
	}

	public void setEditorPart(IEditorPart editorPart) {
		// TODO: more elegant solution to get the file
		// IEditorPart editorPart = PlatformUI.getWorkbench()
		// .getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		// TODO: do I need class checking?
		editor = (ITextEditor) editorPart;
		// TODO: do I need class checking?
		IFileEditorInput input = (IFileEditorInput) (editor.getEditorInput());
		file = input.getFile();
		doc = editor.getDocumentProvider().getDocument(input);
		selection = (ITextSelection) (editor.getSelectionProvider()
				.getSelection());

	}

	public void initSelection() {
		selection = (ITextSelection) (editor.getSelectionProvider()
				.getSelection());
	}

	public void setSelection(ISelection selection) {
		// TODO: do I need class checking?
		this.selection = (ITextSelection) selection;
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

	public Integer getStartLine() {
		startLine = selection.getStartLine() + 1;
		return startLine;
	}

	public Integer getStartColoumn() {
		startColoumn = calculateColoumnFromOffset(selection.getOffset());
		return startColoumn;
	}

	public Integer getEndLine() {
		endLine = selection.getEndLine() + 1;
		return endLine;
	}

	public Integer getEndColoumn() {
		endColoumn = calculateColoumnFromOffset(selection.getOffset()
				+ selection.getLength());
		return endColoumn;
	}

	public IFile getFile() {
		return file;
	}

}
