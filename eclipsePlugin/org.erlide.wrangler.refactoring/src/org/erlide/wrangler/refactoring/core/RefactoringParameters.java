package org.erlide.wrangler.refactoring.core;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Wrangler refactoring parameters. Refactorings which extends
 * <code>WranglerRefactoring</code> class always has an object from this
 * class. It makes easy to get necessary information for the several kinds of
 * refactorings.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RefactoringParameters {

	private ITextEditor editor;
	private IFile file;
	private ITextSelection selection;
	private Integer startLine;
	private Integer startColoumn;
	private Integer endLine;
	private Integer endColoumn;
	private IDocument doc;

	/**
	 * Default constructor. The necessaryinformation is <b>not</b> given by
	 * this constructor, but <code>SetEditorPart</code> and
	 * <code>SetSelection</code> method.
	 */
	public RefactoringParameters() {
		super();
	}

	/**
	 * Calculates the column number from offset.
	 * 
	 * @param offset
	 *            offset of the selected text
	 * @return the column number of the selected text
	 */
	private int calculateColumnFromOffset(int offset) {
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

	/**
	 * @return the active <code>Editor</code>
	 */
	public ITextEditor getEditor() {
		return editor;
	}

	/**
	 * @return the active selection's ending column number
	 */
	public Integer getEndColumn() {
		endColoumn = calculateColumnFromOffset(selection.getOffset()
				+ selection.getLength()) - 1;
		return endColoumn;
	}

	/**
	 * @return the active selection's ending line number
	 */
	public Integer getEndLine() {
		endLine = selection.getEndLine() + 1;
		return endLine;
	}

	/**
	 * @return the file of the active <code>Editor</code>
	 */
	public IFile getFile() {
		return file;
	}

	/**
	 * @return the OS dependent path of the selected file
	 */
	public String getFilePath() {
		return file.getLocation().toOSString();
	}

	/**
	 * @return the Erlang representation of the source directories
	 */
	public OtpErlangList getProject() {
		// FIXME: check another directories as well
		String sPath;

		sPath = file.getParent().getLocation().toOSString();
		final OtpErlangList searchPathList = new OtpErlangList(
				new OtpErlangString(sPath));
		return searchPathList;
	}

	/**
	 * @return the active selection
	 */
	public ITextSelection getSelection() {
		return selection;
	}

	/**
	 * @return the active selection's starting column number
	 */
	public Integer getStartColumn() {
		startColoumn = calculateColumnFromOffset(selection.getOffset());
		return startColoumn;
	}

	/**
	 * @return the active selection's starting line number
	 */
	public Integer getStartLine() {
		startLine = selection.getStartLine() + 1;
		return startLine;
	}

	/**
	 * Initializes the active selection.
	 */
	public void initSelection() {
		selection = (ITextSelection) (editor.getSelectionProvider()
				.getSelection());
	}

	/**
	 * Sets the active editor for the object, then initializes the pending
	 * fields.
	 * 
	 * @param editorPart
	 *            an editor
	 */
	public void setEditorPart(final IEditorPart editorPart) {
		// TODO: do I need class checking?
		editor = (ITextEditor) editorPart;
		// TODO: do I need class checking?
		IFileEditorInput input = (IFileEditorInput) (editor.getEditorInput());
		file = input.getFile();
		doc = editor.getDocumentProvider().getDocument(input);
		selection = (ITextSelection) (editor.getSelectionProvider()
				.getSelection());

	}

	/**
	 * Sets the given selection for the object.
	 * 
	 * @param selection
	 *            active selection
	 */
	public void setSelection(final ISelection selection) {
		// TODO: do I need class checking?
		this.selection = (ITextSelection) selection;
	}
}
