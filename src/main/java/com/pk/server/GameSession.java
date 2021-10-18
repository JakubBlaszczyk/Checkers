package com.pk.server;

import java.io.IOException;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;

import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;

/**
 * 
 */
public interface GameSession extends Callable<Integer> {
 /**
  * Send move to player.
  * @param move move to send.
  * @throws IOException thrown if underlying channel is closed.
  * @throws MoveRejected thrown if player responded in non accepted way.
  */
  public void move(Move move) throws IOException, MoveRejected;
  /**
   * Send message to player.
   * @param msg base64 encoded message to send.
   * @throws IOException thrown if underlying channel is closed.
   */
  public void chatSendMsg(String msg) throws IOException;

  public BlockingQueue<String> getBQueueMsgs();
  public BlockingQueue<Move> getBQueueMoves();
  public Socket getSocket();
}
