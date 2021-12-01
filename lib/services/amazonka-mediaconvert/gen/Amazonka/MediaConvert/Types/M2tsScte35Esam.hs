{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.M2tsScte35Esam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M2tsScte35Esam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for SCTE-35 signals from ESAM. Include this in your job
-- settings to put SCTE-35 markers in your HLS and transport stream outputs
-- at the insertion points that you specify in an ESAM XML document.
-- Provide the document in the setting SCC XML (sccXml).
--
-- /See:/ 'newM2tsScte35Esam' smart constructor.
data M2tsScte35Esam = M2tsScte35Esam'
  { -- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream
    -- generated by ESAM.
    scte35EsamPid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'M2tsScte35Esam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scte35EsamPid', 'm2tsScte35Esam_scte35EsamPid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream
-- generated by ESAM.
newM2tsScte35Esam ::
  M2tsScte35Esam
newM2tsScte35Esam =
  M2tsScte35Esam' {scte35EsamPid = Prelude.Nothing}

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream
-- generated by ESAM.
m2tsScte35Esam_scte35EsamPid :: Lens.Lens' M2tsScte35Esam (Prelude.Maybe Prelude.Natural)
m2tsScte35Esam_scte35EsamPid = Lens.lens (\M2tsScte35Esam' {scte35EsamPid} -> scte35EsamPid) (\s@M2tsScte35Esam' {} a -> s {scte35EsamPid = a} :: M2tsScte35Esam)

instance Core.FromJSON M2tsScte35Esam where
  parseJSON =
    Core.withObject
      "M2tsScte35Esam"
      ( \x ->
          M2tsScte35Esam'
            Prelude.<$> (x Core..:? "scte35EsamPid")
      )

instance Prelude.Hashable M2tsScte35Esam where
  hashWithSalt salt' M2tsScte35Esam' {..} =
    salt' `Prelude.hashWithSalt` scte35EsamPid

instance Prelude.NFData M2tsScte35Esam where
  rnf M2tsScte35Esam' {..} = Prelude.rnf scte35EsamPid

instance Core.ToJSON M2tsScte35Esam where
  toJSON M2tsScte35Esam' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scte35EsamPid" Core..=)
              Prelude.<$> scte35EsamPid
          ]
      )
