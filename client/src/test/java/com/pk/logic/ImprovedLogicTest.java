package com.pk.logic;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.pk.frontend.checkers.MoveResult;
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
      assertEquals(new MoveResult(MoveType.NONE).getType(), logic.update(0, 0, 1, 1).getType());
      log.debug("\n{}", logic.toString());
      log.debug("NONE");
      assertEquals(new MoveResult(MoveType.NONE).getType(), logic.update(2, 1, 1, 1).getType());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(2, 2, 1, 1).getType());
      log.debug("\n{}", logic.toString());
      log.debug("NONE");
      assertEquals(new MoveResult(MoveType.NONE).getType(), logic.update(2, 2, 1, 1).getType());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(5, 5, 6, 6).getType());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(3, 3, 2, 2).getType());
      log.debug("\n{}", logic.toString());
      log.debug("NONE");
      assertEquals(new MoveResult(MoveType.NONE).getType(), logic.update(3, 3, 2, 2).getType());
      log.debug("\n{}", logic.toString());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(6, 6, 7, 7).getType());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(1, 1, 0, 0).getType());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(4, 4, 5, 5).getType());
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.MANDATORY_KILL).getType(), logic.update(2, 4, 3, 3).getType());
      log.debug("KILL");
      assertEquals(new MoveResult(MoveType.KILL).getType(), logic.update(5, 5, 3, 3).getType());
      log.debug("\n{}", logic.toString());
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.MANDATORY_KILL).getType(), logic.update(0, 2, 1, 1).getType());
      log.debug("KILL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.KILL).getType(), logic.update(7, 7, 5, 5).getType());
      log.debug("\n{}", logic.toString());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(0, 2, 1, 1).getType());
      log.debug("NORMAL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.NORMAL).getType(), logic.update(5, 5, 4, 6).getType());
      log.debug("\n{}", logic.toString());
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.MANDATORY_KILL).getType(), logic.update(6, 6, 7, 7).getType());
      log.debug("MANDATORY_KILL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.MANDATORY_KILL).getType(), logic.update(1, 3, 0, 2).getType());
      log.debug("KILL {}", logic.getTurn().toString());
      assertEquals(new MoveResult(MoveType.KILL).getType(), logic.update(0, 0, 7, 7).getType());
    } catch (Exception e) {
      log.error("Messegu", e);
    }
  }
}
