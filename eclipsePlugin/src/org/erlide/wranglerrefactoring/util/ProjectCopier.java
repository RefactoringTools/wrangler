package org.erlide.wranglerrefactoring.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;

//lifecycle constructor(...)->doCopy()/...->createChanges()/->dispose()
/**
 * TODO:: should it be static??? R U sure?
 * 
 * @author mee
 * 
 */
public class ProjectCopier {

	private IProject project;

	private IFile refacFile;

	private File refacFileCopy;

	private File tmpFolder;

	private ArrayList<FileTuplesResource> files;

	public ProjectCopier(IFile refacFile) {
		this.refacFile = refacFile;
		this.project = refacFile.getProject();
		tmpFolder = null;
		files = new ArrayList<FileTuplesResource>();
	}

	public void doCopy() throws IOException, CoreException {
		tmpFolder = createTmpDir();
		getMembers(project);
		for (FileTuplesResource f : files) {
			f.createCopy();
			if (f.getEclipseFile().equals(refacFile)) {
				refacFileCopy = f.getFileCopy();
			}
		}
	}

	// TODO: check preconditions
	public String getSearchPath() throws IOException {
		return tmpFolder.getCanonicalPath();
	}

	public String getFilePath() throws IOException {
		return refacFileCopy.getCanonicalPath().toString();
	}

	public Change createChanges() throws IOException, CoreException {
		CompositeChange change = new CompositeChange(project.getName());

		for (FileTuplesResource f : files) {
			change.add(f.createChanges());
		}

		// dispose();

		return change;
	}

	// TODO: is it general enough??? do we need IContainer instead of IFolder
	private void getMembers(IContainer container) throws CoreException {
		IResource[] res = container.members();
		for (IResource r : res) {
			if (r instanceof IFolder) {
				IContainer c = (IContainer) r;
				getMembers(c);
			} else if (r instanceof IFile) {
				IFile f = (IFile) r;
				if (isErlangFile(f))
					files.add(new FileTuplesResource(f, tmpFolder));
			}
		}
	}

	protected boolean isErlangFile(IResource f) {
		String fileExtension = f.getFileExtension();
		if (fileExtension != null)
			if (fileExtension.equals("erl") || fileExtension.equals("hrl"))
				return true;
		return false;
	}

	/*
	 * protected void addFile(IFile file) throws IOException{
	 * if(tmpFolder==null) tmpFolder = createTmpDir(); files.add(new
	 * WranglerFileResource(file, tmpFolder)); }
	 */

	protected File createTmpDir() throws IOException {
		File tmp = java.io.File.createTempFile(project.getName() + "_",
				"_wranglerTmpCopy");

		if (!tmp.delete())
			throw new IOException("could not delete temporary file");
		if (!tmp.mkdir())
			throw new IOException("could not create temporary directory");

		return tmp;
	}

	public void dispose() {
		if (files != null) {
			for (FileTuplesResource f : files) {
				f.dispose();
			}
			files = null;
		}
		if (tmpFolder != null) {
			tmpFolder.delete();
			tmpFolder = null;
		}

	}

}
