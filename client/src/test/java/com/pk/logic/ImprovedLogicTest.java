package com.pk.logic;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.pk.frontend.checkers.MoveType;

import org.junit.Test;
import org.junit.jupiter.api.Order;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ImprovedLogicTest {

  @Order(0)
  @Test
  public void checkBoardNormalMove() {
    try {
      ImprovedLogic logic = new ImprovedLogic(8);
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(0, 0, 1, 1));
      log.debug("{}", logic.toString());
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(2, 1, 1, 1));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(2, 2, 1, 1));
      log.debug("{}", logic.toString());
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(2, 2, 1, 1));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(5, 5, 6, 6));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(3, 3, 2, 2));
      log.debug("NONE");
      assertEquals(MoveType.NONE, logic.update(3, 3, 2, 2));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(6, 6, 7, 7));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(1, 1, 0, 0));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(4, 4, 5, 5));
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(MoveType.MANDATORY_KILL, logic.update(2, 4, 3, 3));
      log.debug("KILL");
      assertEquals(MoveType.KILL, logic.update(5, 5, 3, 3));
      log.debug("{}", logic.toString());
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(MoveType.MANDATORY_KILL, logic.update(0, 2, 1, 1));
      log.debug("KILL {}", logic.getTurn().toString());
      assertEquals(MoveType.KILL, logic.update(7, 7, 5, 5));
      log.debug("{}", logic.toString());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(0, 2, 1, 1));
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(MoveType.NORMAL, logic.update(5, 5, 4, 6));
      log.debug("{}", logic.toString());
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(MoveType.MANDATORY_KILL, logic.update(6, 6, 7, 7));
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(MoveType.MANDATORY_KILL, logic.update(1, 3, 0, 2));
      log.debug("KILL {}", logic.getTurn().toString());
      assertEquals(MoveType.KILL, logic.update(0, 0, 7, 7));
    } catch (Exception e) {
      log.error("Messegu", e);
    }
  }
}
