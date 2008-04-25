package org.erlide.wranglerrefactoring.core.exception;

public class WranglerRPCException extends WranglerException {

	public WranglerRPCException() {
		super("Could not reach the Erlang node!");
	}

}
