package com.pk;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.InvalidAlgorithmParameterException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ThreadLocalRandom;

import javax.management.RuntimeErrorException;

import com.pk.models.Config;
import com.pk.models.Player;

import org.apache.commons.collections4.BidiMap;
import org.apache.commons.collections4.bidimap.DualHashBidiMap;

import lombok.extern.slf4j.Slf4j;

// FIXME detect already closed players, another thread (???)
// FIXME standarize messages, it's mess atm
@Slf4j
public class Server implements Callable<Integer> {
  // private @Getter @Setter @NonNull BlockingQueue<Invite> bQueue;
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  private Map<SocketChannel, SocketChannel> playersInGames;
  private BidiMap<String, SocketChannel> connectedPlayersMap;
  private BidiMap<SelectionKey, SocketChannel> tmpMap;
  private Set<Player> hashPlayers;

  public Server(String ip, Integer port, Map<SocketChannel, SocketChannel> playersInGames,
      BidiMap<String, SocketChannel> connectedPlayersMap, Set<Player> hashPlayers) throws IOException {
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(ip, port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    this.playersInGames = playersInGames;
    this.connectedPlayersMap = connectedPlayersMap;
    this.hashPlayers = hashPlayers;
    tmpMap = new DualHashBidiMap<>();
  }

  public void cleanup() throws IOException {
    serverSocketChannel.close();
    selector.close();
  }

  // FIXME refactor this monster
  @Override
  public Integer call() throws Exception {
    SelectionKey key = null;
    while (true) {
      if (Thread.currentThread().isInterrupted()) {
        return 0;
      }
      if (selector.select() <= 0)
        continue;
      Set<SelectionKey> set = selector.selectedKeys();
      Set<SelectionKey> set2 = new HashSet<SelectionKey>(set); 
      Iterator<SelectionKey> iterator = set.iterator();
      while (iterator.hasNext()) {
        key = iterator.next();
        iterator.remove();
        if (key.isAcceptable()) {
          SocketChannel sc = serverSocketChannel.accept();
          sc.configureBlocking(false);
          SelectionKey tmp = sc.register(selector, SelectionKey.OP_READ);
          tmpMap.put(tmp, sc);
          log.info("Connection Accepted: " + sc.getLocalAddress());
        }
        if (key.isReadable()) {
          // selector.
          // FIXME add method to do it
          SocketChannel sc = (SocketChannel) key.channel();
          ByteBuffer bb = ByteBuffer.allocate(1024);
          int len = sc.read(bb);
          if (len <= 0) {
            sc.close();
            log.info("Connection closed");
            continue;
          }
          String result = new String(bb.array(), 0, len).strip();
          log.info("Message received: " + result + " \nMessage length: " + result.length());
          if (result.startsWith("checkers:config ")) {
            if (connectedPlayersMap.containsValue(sc)) {
              sc.write(ByteBuffer.wrap("checkers:error user already configured".getBytes()));
            }
            Optional<Config> conf = parseConfigMessage(result.substring(16));
            if (conf.isEmpty()) {
              log.error("Got invalid config message, ignoring");
              continue;
            }
            if (connectedPlayersMap.containsKey(conf.get().getNickname())) {
              log.warn("User already present on server: " + conf.get().getNickname());
              sc.write(ByteBuffer.wrap("checkers:error user already present".getBytes()));
              continue;
            }
            connectedPlayersMap.put(conf.get().getNickname(), sc);
            hashPlayers.add(new Player(conf.get().getNickname(), conf.get().getProfileImg()));
            ByteBuffer src = ByteBuffer.wrap(transformPlayersListIntoBytes());
            sc.write(src);
          } else if (result.equals("checkers:randomGame")) {
            if (!connectedPlayersMap.containsValue(sc)) {
              sc.write(ByteBuffer.wrap("checkers:error user not configured".getBytes()));
            }
            // FIXME add random game setup
            key.cancel();
            // Optional<List<Socket>> sockets = findPlayersToRandomGame(sc, selector.selectedKeys().iterator());
            Optional<List<Socket>> sockets = findPlayersToRandomGame(sc, set2);
            if (sockets.isEmpty()) {
              sc.write(ByteBuffer.wrap("checkers:no players are active :(".getBytes()));
              continue;
            }
            // FIXME XD
            List<Socket> tmp = sockets.get();
            Thread th = new Thread(new SessionHandler(connectedPlayersMap.getKey(tmp.get(0)),
                connectedPlayersMap.getKey(tmp.get(1)), tmp.get(0), tmp.get(1), connectedPlayersMap));
            th.start();
            tmp.get(0).getOutputStream().write("checkers:randomStart".getBytes());
            tmp.get(0).getOutputStream().write("checkers:randomStart".getBytes());
          } else {
            log.warn("Got invalid message");
          }
        }
      }
    }
  }

  // FIXME refactor it
  // FIXME prevent calling to game already playing guy
  // private Optional<List<Socket>> findPlayersToRandomGame(SocketChannel src, Iterator<SelectionKey> iterator) {
  private Optional<List<Socket>> findPlayersToRandomGame(SocketChannel src, Set<SelectionKey> set2) {
    if (connectedPlayersMap.size() == 1) {
      return Optional.empty();
    }
    String srcNick = connectedPlayersMap.getKey(src);
    SocketChannel dst = null;
    int idx = 0;
    boolean flag = false;
    List<Integer> blacklist = new ArrayList<>();
    for (;;) {
      if (flag) {
        break;
      }
      idx = 0;
      int random = ThreadLocalRandom.current().nextInt(0, connectedPlayersMap.size());
      if (blacklist.contains(random)) {
        continue;
      }
      for (SocketChannel sc : connectedPlayersMap.values()) {
        if (idx == random) {
          if (sc == src) {
            if (random == connectedPlayersMap.size() - 1) {
              blacklist.add(random);
              break;
            } else {
              random++;
            }
            idx++;
            continue;
          }
          dst = sc;
          flag = true;
          break;
        }
        idx++;
      }
    }
    // Iterator<SelectionKey> iterator = set2.iterator();
    // while (iterator.hasNext()) {
    //   SelectionKey sk = iterator.next();
    //   if (sk.channel() == dst) {
    //     log.info("Found selection key");
    //     sk.cancel();
    //     break;
    //   }
    // }
    SelectionKey key = tmpMap.getKey(dst);
    key.cancel();
    try {
    src.configureBlocking(true);
    // Zdycha
    dst.configureBlocking(true);
    } catch (IOException e) {
    log.error("configureBlocking throw IOException ???", e);
    return Optional.empty();
    }
    return Optional.of(List.of(src.socket(), dst.socket()));
  }

  private byte[] transformPlayersListIntoBytes() {
    StringBuilder sb = new StringBuilder();
    sb.append("checkers:onlinePlayers ");
    for (Player player : hashPlayers) {
      sb.append(player.toSendable());
      sb.append(" ");
    }
    return sb.toString().strip().getBytes();
  }

  private Optional<Config> parseConfigMessage(String msg) {
    try {
      String[] parts = msg.split(" ");
      // FIXME magic number
      if (parts.length != 2) {
        throw new InvalidAlgorithmParameterException(
            String.format("Invalid len, should be: {}, is {}", 2, parts.length));
      }
      // FIXME add b64 handling
      return Optional.of(new Config(parts[0], parts[1]));
    } catch (Exception e) {
      return Optional.empty();
    }
  }
}
