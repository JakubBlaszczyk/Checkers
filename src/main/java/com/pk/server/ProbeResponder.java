package com.pk.server;

public interface ProbeResponder extends Runnable {
  public void setNick(String nick);
  public void setProfileImg(String profileImg);
  public String getNick();
  public String getProfileImg();
}
