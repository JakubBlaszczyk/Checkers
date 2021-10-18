package com.pk.server;

import java.util.concurrent.Callable;

/**
 * 
 */
public interface ProbeResponder extends Callable<Integer> {
  public void setNick(String nick);

  public void setProfileImg(String profileImg);
}
