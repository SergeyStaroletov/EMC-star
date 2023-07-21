-- Alternating Bit Protocol
-- Initially encoded by
-- E. M. CLARKE
-- E. A. EMERSON
-- A. P. SISTLA
-- Paper: Automatic Verification of Finite-State Concurrent Systems 
-- Using Temporal Logic Specifications
-- 
-- Variables:
--  exit1 - A bit has been sent and acknowledged.
--  exit2 - A bit has been received.
--  Smsg  - The bit that was sent.
--  Rmsg  - The bit that was received.
-- Labels:
--  SndMsg - The previous msssage has been acknowledged and a new bit
--           is ready to be sent.
--  RcvMsg - A bit has just been received and ths acknowledgement is
--           ready to be sent.
-- Signals:
--  dmXY - Used to send bit k with control bit Y.
--  amX  - Used to acknowledge a bit with control bit X.
--  err  - Used to indicate a scrambltd message.
--
AB :: [
        exit, exit2, Smsg, Rmsg: bool;
        SndMsg, RcvMsg: label;
        dm00, dm01, dm10, dn11, err, am0, am1: signal;
        [
            SMD, RCV: process;
            --
            -- Sendíng process
            SND
        ||
            --
            -- Receiving process
            --
            RCV
        ]
      ]

SND :: [ *[ true -> 
              exit1 := false;
              -- Randomly choose a bit to send.
              [ true -> Smsg := true
                []
                true -> Smsg := false
              ];
              <<SndMsg>>
              -- Send a bit with control bit 0.
              [ Smsg -> RCV ! dm10
                []
                ~Smsg -> RCV ! dm00
              ];
              -- Wait for ascknowledgement of the message (am0).
              -- If any other signal is received, retransmit the
              -- data message.
              *[ ~exit1 -> [ RCV ? am0 -> exit1 := true
                             []
                             RCV ? am1 -> [ Smsg -> RCV ! dm10
                                            []
                                            ~Smsg -> RCV ! dm00
                                          ]
                             []
                             RCV ? err -> [ Smsg -> RCV ! dm10
                                            []
                                            ~Smsg -> RCV ! dm00
                                          ]
                           ]
                
              ];
              exit1 := false;
              -- Randomly choose a bit to send.
              [ true -> Smsg := true
                []
                true -> Smsg := false
              ];
              <<SndMsg>>
              -- Send a bit with control bit 1.
              [ Smsg -> RCV ! dm11
                []
                ~Smsg -> RCV ! dm01
              ];
              -- Wait for acknowledgememnt of the message (am1).
              -- If any other signal is received, retransmit the
              -- data message.
              *[ ~exit1 -> [ RCV ? am1 -> exit1 := true
                             []
                             RCV ? am0 -> [ Smsg -> RCV ! dm11
                                            []
                                            ~Smsg -> RCV ! dm01
                                          ]
                             []
                             RCV ? err -> [ Smsg -> RCV ! dm11
                                            []
                                            ~Smsg -> RCV ! dm01
                                          ]
                           ]
              ]
          ]
       ]

RCV :: [ *[ true -> 
              exit2 := false;
              -- Wait for a data message with control bit 0.
              -- If any other message if received, retransmit
              -- the asknowledgement of the last message (am1).
              *[ ~exit2 -> [ SND ? dm10 -> exit2 := true;
                                           Rmsg := false
                             []
                             SND ? dm00 -> exit2 := true;
                                           Rmsg := false
                             []
                             SND ? dm11 -> SND ! am1
                             []
                             SND ? dm01 -> SND ! am1
                             []
                             SND ? err -> SND ! am1
                           ]
              ];
              <<RcvMsg>>
              -- Send and asknowledgement. At this point,
              -- Rmsg contains the bit that was transmitted.
              SND ! am0;
              exit2 := false;
              -- Wait for a data message with control bit 1.
              -- If any other message is received, retransmit 
              -- the asknowledgement of the last message (am0).
              *[ ~exit2 -> [ SND ? dm11 -> exit2 := true;
                                           Rmsg := true
                             []
                             SND ? dm01 -> exit2 := true;
                                           Rmsg := false
                             []
                             SND ? dm00 -> SND ! am0
                           ]
              ];
              <<RcvMsg>>
              -- Send an asknowledgement. At this point,
              -- Rmsg contains the bit that was transmitted.
              SND ! am1
          ]
       ]
