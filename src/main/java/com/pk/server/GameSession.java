package com.pk.server;

import java.io.IOException;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;

import com.pk.server.exceptions.ChatMsgRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;

public interface GameSession extends Callable<Integer> {
  public void move(Move move) throws IOException, MoveRejected;
  public void chatSendMsg(String msg) throws IOException, ChatMsgRejected;

  public BlockingQueue<String> getBQueueMsgs();
  public BlockingQueue<Move> getBQueueMoves();
}
