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
-- Module      : Amazonka.Inspector.Types.AssessmentRunAgent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentRunAgent where

import qualified Amazonka.Core as Core
import Amazonka.Inspector.Types.AgentHealth
import Amazonka.Inspector.Types.AgentHealthCode
import Amazonka.Inspector.Types.TelemetryMetadata
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Inspector agent. This data type is
-- used as a response element in the ListAssessmentRunAgents action.
--
-- /See:/ 'newAssessmentRunAgent' smart constructor.
data AssessmentRunAgent = AssessmentRunAgent'
  { -- | The Auto Scaling group of the EC2 instance that is specified by the
    -- agent ID.
    autoScalingGroup :: Prelude.Maybe Prelude.Text,
    -- | The description for the agent health code.
    agentHealthDetails :: Prelude.Maybe Prelude.Text,
    -- | The AWS account of the EC2 instance where the agent is installed.
    agentId :: Prelude.Text,
    -- | The ARN of the assessment run that is associated with the agent.
    assessmentRunArn :: Prelude.Text,
    -- | The current health state of the agent.
    agentHealth :: AgentHealth,
    -- | The detailed health state of the agent.
    agentHealthCode :: AgentHealthCode,
    -- | The Amazon Inspector application data metrics that are collected by the
    -- agent.
    telemetryMetadata :: [TelemetryMetadata]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentRunAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroup', 'assessmentRunAgent_autoScalingGroup' - The Auto Scaling group of the EC2 instance that is specified by the
-- agent ID.
--
-- 'agentHealthDetails', 'assessmentRunAgent_agentHealthDetails' - The description for the agent health code.
--
-- 'agentId', 'assessmentRunAgent_agentId' - The AWS account of the EC2 instance where the agent is installed.
--
-- 'assessmentRunArn', 'assessmentRunAgent_assessmentRunArn' - The ARN of the assessment run that is associated with the agent.
--
-- 'agentHealth', 'assessmentRunAgent_agentHealth' - The current health state of the agent.
--
-- 'agentHealthCode', 'assessmentRunAgent_agentHealthCode' - The detailed health state of the agent.
--
-- 'telemetryMetadata', 'assessmentRunAgent_telemetryMetadata' - The Amazon Inspector application data metrics that are collected by the
-- agent.
newAssessmentRunAgent ::
  -- | 'agentId'
  Prelude.Text ->
  -- | 'assessmentRunArn'
  Prelude.Text ->
  -- | 'agentHealth'
  AgentHealth ->
  -- | 'agentHealthCode'
  AgentHealthCode ->
  AssessmentRunAgent
newAssessmentRunAgent
  pAgentId_
  pAssessmentRunArn_
  pAgentHealth_
  pAgentHealthCode_ =
    AssessmentRunAgent'
      { autoScalingGroup =
          Prelude.Nothing,
        agentHealthDetails = Prelude.Nothing,
        agentId = pAgentId_,
        assessmentRunArn = pAssessmentRunArn_,
        agentHealth = pAgentHealth_,
        agentHealthCode = pAgentHealthCode_,
        telemetryMetadata = Prelude.mempty
      }

-- | The Auto Scaling group of the EC2 instance that is specified by the
-- agent ID.
assessmentRunAgent_autoScalingGroup :: Lens.Lens' AssessmentRunAgent (Prelude.Maybe Prelude.Text)
assessmentRunAgent_autoScalingGroup = Lens.lens (\AssessmentRunAgent' {autoScalingGroup} -> autoScalingGroup) (\s@AssessmentRunAgent' {} a -> s {autoScalingGroup = a} :: AssessmentRunAgent)

-- | The description for the agent health code.
assessmentRunAgent_agentHealthDetails :: Lens.Lens' AssessmentRunAgent (Prelude.Maybe Prelude.Text)
assessmentRunAgent_agentHealthDetails = Lens.lens (\AssessmentRunAgent' {agentHealthDetails} -> agentHealthDetails) (\s@AssessmentRunAgent' {} a -> s {agentHealthDetails = a} :: AssessmentRunAgent)

-- | The AWS account of the EC2 instance where the agent is installed.
assessmentRunAgent_agentId :: Lens.Lens' AssessmentRunAgent Prelude.Text
assessmentRunAgent_agentId = Lens.lens (\AssessmentRunAgent' {agentId} -> agentId) (\s@AssessmentRunAgent' {} a -> s {agentId = a} :: AssessmentRunAgent)

-- | The ARN of the assessment run that is associated with the agent.
assessmentRunAgent_assessmentRunArn :: Lens.Lens' AssessmentRunAgent Prelude.Text
assessmentRunAgent_assessmentRunArn = Lens.lens (\AssessmentRunAgent' {assessmentRunArn} -> assessmentRunArn) (\s@AssessmentRunAgent' {} a -> s {assessmentRunArn = a} :: AssessmentRunAgent)

-- | The current health state of the agent.
assessmentRunAgent_agentHealth :: Lens.Lens' AssessmentRunAgent AgentHealth
assessmentRunAgent_agentHealth = Lens.lens (\AssessmentRunAgent' {agentHealth} -> agentHealth) (\s@AssessmentRunAgent' {} a -> s {agentHealth = a} :: AssessmentRunAgent)

-- | The detailed health state of the agent.
assessmentRunAgent_agentHealthCode :: Lens.Lens' AssessmentRunAgent AgentHealthCode
assessmentRunAgent_agentHealthCode = Lens.lens (\AssessmentRunAgent' {agentHealthCode} -> agentHealthCode) (\s@AssessmentRunAgent' {} a -> s {agentHealthCode = a} :: AssessmentRunAgent)

-- | The Amazon Inspector application data metrics that are collected by the
-- agent.
assessmentRunAgent_telemetryMetadata :: Lens.Lens' AssessmentRunAgent [TelemetryMetadata]
assessmentRunAgent_telemetryMetadata = Lens.lens (\AssessmentRunAgent' {telemetryMetadata} -> telemetryMetadata) (\s@AssessmentRunAgent' {} a -> s {telemetryMetadata = a} :: AssessmentRunAgent) Prelude.. Lens.coerced

instance Core.FromJSON AssessmentRunAgent where
  parseJSON =
    Core.withObject
      "AssessmentRunAgent"
      ( \x ->
          AssessmentRunAgent'
            Prelude.<$> (x Core..:? "autoScalingGroup")
            Prelude.<*> (x Core..:? "agentHealthDetails")
            Prelude.<*> (x Core..: "agentId")
            Prelude.<*> (x Core..: "assessmentRunArn")
            Prelude.<*> (x Core..: "agentHealth")
            Prelude.<*> (x Core..: "agentHealthCode")
            Prelude.<*> ( x Core..:? "telemetryMetadata"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AssessmentRunAgent where
  hashWithSalt salt' AssessmentRunAgent' {..} =
    salt' `Prelude.hashWithSalt` telemetryMetadata
      `Prelude.hashWithSalt` agentHealthCode
      `Prelude.hashWithSalt` agentHealth
      `Prelude.hashWithSalt` assessmentRunArn
      `Prelude.hashWithSalt` agentId
      `Prelude.hashWithSalt` agentHealthDetails
      `Prelude.hashWithSalt` autoScalingGroup

instance Prelude.NFData AssessmentRunAgent where
  rnf AssessmentRunAgent' {..} =
    Prelude.rnf autoScalingGroup
      `Prelude.seq` Prelude.rnf telemetryMetadata
      `Prelude.seq` Prelude.rnf agentHealthCode
      `Prelude.seq` Prelude.rnf agentHealth
      `Prelude.seq` Prelude.rnf assessmentRunArn
      `Prelude.seq` Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf agentHealthDetails
