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
-- Module      : Amazonka.CloudTrail.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the type and name of a resource referenced by an event.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The type of a resource referenced by the event returned. When the
    -- resource type cannot be determined, null is returned. Some examples of
    -- resource types are: __Instance__ for EC2, __Trail__ for CloudTrail,
    -- __DBInstance__ for Amazon RDS, and __AccessKey__ for IAM. To learn more
    -- about how to look up and filter events by the resource types supported
    -- for a service, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events>.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource referenced by the event returned. These are
    -- user-created names whose values will depend on the environment. For
    -- example, the resource name might be \"auto-scaling-test-group\" for an
    -- Auto Scaling Group or \"i-1234567\" for an EC2 Instance.
    resourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resource_resourceType' - The type of a resource referenced by the event returned. When the
-- resource type cannot be determined, null is returned. Some examples of
-- resource types are: __Instance__ for EC2, __Trail__ for CloudTrail,
-- __DBInstance__ for Amazon RDS, and __AccessKey__ for IAM. To learn more
-- about how to look up and filter events by the resource types supported
-- for a service, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events>.
--
-- 'resourceName', 'resource_resourceName' - The name of the resource referenced by the event returned. These are
-- user-created names whose values will depend on the environment. For
-- example, the resource name might be \"auto-scaling-test-group\" for an
-- Auto Scaling Group or \"i-1234567\" for an EC2 Instance.
newResource ::
  Resource
newResource =
  Resource'
    { resourceType = Prelude.Nothing,
      resourceName = Prelude.Nothing
    }

-- | The type of a resource referenced by the event returned. When the
-- resource type cannot be determined, null is returned. Some examples of
-- resource types are: __Instance__ for EC2, __Trail__ for CloudTrail,
-- __DBInstance__ for Amazon RDS, and __AccessKey__ for IAM. To learn more
-- about how to look up and filter events by the resource types supported
-- for a service, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events>.
resource_resourceType :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceType = Lens.lens (\Resource' {resourceType} -> resourceType) (\s@Resource' {} a -> s {resourceType = a} :: Resource)

-- | The name of the resource referenced by the event returned. These are
-- user-created names whose values will depend on the environment. For
-- example, the resource name might be \"auto-scaling-test-group\" for an
-- Auto Scaling Group or \"i-1234567\" for an EC2 Instance.
resource_resourceName :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceName = Lens.lens (\Resource' {resourceName} -> resourceName) (\s@Resource' {} a -> s {resourceName = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "ResourceName")
      )

instance Prelude.Hashable Resource where
  hashWithSalt salt' Resource' {..} =
    salt' `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceName
