package org.erlide.wranglerrefactoring.ui;

import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.wranglerrefactoring.core.rename.RenameRefactoring;

public abstract class WranglerNewNameInputPage extends UserInputWizardPage {

	protected Label renameLabel;
	protected Text newNameText;
	protected Composite composite;

	protected String refactoringName;

	public WranglerNewNameInputPage(String name) {
		super(name);

		refactoringName = name;
	}

	@Override
	public void createControl(Composite parent) {

		setDescription(initDescription());
		setTitle(initTitle());

		composite = new Composite(parent, SWT.NONE);

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;

		composite.setLayout(layout);

		renameLabel = new Label(composite, SWT.LEFT);
		renameLabel.setText(initLabelText());
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		renameLabel.setLayoutData(gridData);

		newNameText = new Text(composite, SWT.NONE);
		gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		newNameText.setLayoutData(gridData);

		initnewNameModifyListener();
		initListeners();
		setPageComplete(false);
		setControl(composite);

	}

	protected void initnewNameModifyListener() {
		newNameText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				RenameRefactoring refac = (RenameRefactoring) getRefactoring();
				refac.setNewName(newNameText.getText());
			}

		});
	}

	protected abstract String initLabelText();

	protected abstract String initTitle();

	protected abstract String initDescription();

	/**
	 * Should be implemented to set listeners.
	 */
	protected abstract void initListeners();

}
