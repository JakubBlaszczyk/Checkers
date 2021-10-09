package com.pk.server;

import java.io.IOException;

import com.pk.server.models.Move;

public interface GameSession {
  public void move(Move move) throws IOException;
  public void chatSendMsg(String msg) throws IOException;
}
