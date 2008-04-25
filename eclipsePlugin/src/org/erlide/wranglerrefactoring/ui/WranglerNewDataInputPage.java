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
import org.erlide.wranglerrefactoring.core.WranglerRefactoring;

public abstract class WranglerNewDataInputPage extends UserInputWizardPage {

	protected Label renameLabel;
	protected Text newDataText;
	protected Composite composite;

	protected String refactoringName;

	public WranglerNewDataInputPage(String name) {
		super(name);

		refactoringName = name;
	}

	@Override
	public void createControl(Composite parent) {

		setDescription(initDescription());
		setTitle(initTitle());

		composite = new Composite(parent, SWT.NONE);

		renameLabel = new Label(composite, SWT.LEFT);
		renameLabel.setText(initLabelText());
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		renameLabel.setLayoutData(gridData);

		newDataText = new Text(composite, SWT.NONE);
		gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		newDataText.setLayoutData(gridData);

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		initExtraControls(layout);
		composite.setLayout(layout);

		initNewNameModifyListener();
		initListeners();
		setPageComplete(false);
		setControl(composite);

	}

	abstract protected void initExtraControls(GridLayout layout);

	protected void initNewNameModifyListener() {
		newDataText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				WranglerRefactoring refac = (WranglerRefactoring) getRefactoring();
				refac.setNewName(newDataText.getText());
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
