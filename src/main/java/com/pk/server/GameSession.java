package com.pk.server;

import java.io.IOException;

import com.pk.server.exceptions.ChatMsgRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;

public interface GameSession {
  public void move(Move move) throws IOException, MoveRejected;
  public void chatSendMsg(String msg) throws IOException, ChatMsgRejected;
}
