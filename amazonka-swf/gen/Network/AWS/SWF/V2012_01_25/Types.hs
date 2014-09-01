{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
-- applications that coordinate work across distributed components. In Amazon
-- SWF, a task represents a logical unit of work that is performed by a
-- component of your application. Coordinating tasks across the application
-- involves managing intertask dependencies, scheduling, and concurrency in
-- accordance with the logical flow of the application. Amazon SWF gives you
-- full control over implementing tasks and coordinating them without worrying
-- about underlying complexities such as tracking their progress and
-- maintaining their state.
module Network.AWS.SWF.V2012_01_25.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V4
import Network.AWS.SWF.V2012_01_25.CountClosedWorkflowExecutions
import Network.AWS.SWF.V2012_01_25.CountOpenWorkflowExecutions
import Network.AWS.SWF.V2012_01_25.CountPendingActivityTasks
import Network.AWS.SWF.V2012_01_25.CountPendingDecisionTasks
import Network.AWS.SWF.V2012_01_25.DeprecateActivityType
import Network.AWS.SWF.V2012_01_25.DeprecateDomain
import Network.AWS.SWF.V2012_01_25.DeprecateWorkflowType
import Network.AWS.SWF.V2012_01_25.DescribeActivityType
import Network.AWS.SWF.V2012_01_25.DescribeDomain
import Network.AWS.SWF.V2012_01_25.DescribeWorkflowExecution
import Network.AWS.SWF.V2012_01_25.DescribeWorkflowType
import Network.AWS.SWF.V2012_01_25.GetWorkflowExecutionHistory
import Network.AWS.SWF.V2012_01_25.ListActivityTypes
import Network.AWS.SWF.V2012_01_25.ListClosedWorkflowExecutions
import Network.AWS.SWF.V2012_01_25.ListDomains
import Network.AWS.SWF.V2012_01_25.ListOpenWorkflowExecutions
import Network.AWS.SWF.V2012_01_25.ListWorkflowTypes
import Network.AWS.SWF.V2012_01_25.PollForActivityTask
import Network.AWS.SWF.V2012_01_25.PollForDecisionTask
import Network.AWS.SWF.V2012_01_25.RecordActivityTaskHeartbeat
import Network.AWS.SWF.V2012_01_25.RegisterActivityType
import Network.AWS.SWF.V2012_01_25.RegisterDomain
import Network.AWS.SWF.V2012_01_25.RegisterWorkflowType
import Network.AWS.SWF.V2012_01_25.RequestCancelWorkflowExecution
import Network.AWS.SWF.V2012_01_25.RespondActivityTaskCanceled
import Network.AWS.SWF.V2012_01_25.RespondActivityTaskCompleted
import Network.AWS.SWF.V2012_01_25.RespondActivityTaskFailed
import Network.AWS.SWF.V2012_01_25.RespondDecisionTaskCompleted
import Network.AWS.SWF.V2012_01_25.SignalWorkflowExecution
import Network.AWS.SWF.V2012_01_25.StartWorkflowExecution
import Network.AWS.SWF.V2012_01_25.TerminateWorkflowExecution
import Network.AWS.SWF.V2012_01_25.Types

-- | Supported version (@2012-01-25@) of the
-- @Amazon Simple Workflow Service@ service.
data SWF deriving (Typeable)

instance AWSService SWF where
    type Sg SWF = V4
    data Er SWF
        = DefaultUndefinedFault
            { _dufMessage :: Maybe Text
            }
        | DomainAlreadyExistsFault
            { _daefMessage :: Maybe Text
            }
        | DomainDeprecatedFault
            { _ddfMessage :: Maybe Text
            }
        | LimitExceededFault
            { _lefMessage :: Maybe Text
            }
        | OperationNotPermittedFault
            { _onpfMessage :: Maybe Text
            }
        | SWFClient HttpException
        | SWFSerializer String
        | SWFService String
        | TypeAlreadyExistsFault
            { _taefMessage :: Maybe Text
            }
        | TypeDeprecatedFault
            { _tdfMessage :: Maybe Text
            }
        | UnknownResourceFault
            { _urfMessage :: Maybe Text
            }
        | WorkflowExecutionAlreadyStartedFault
            { _weasfMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "swf"
        , _svcVersion  = "2012-01-25"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er SWF)
deriving instance Generic (Er SWF)

instance AWSError (Er SWF) where
    awsError = const "SWFError"

instance AWSServiceError (Er SWF) where
    serviceError    = SWFService
    clientError     = SWFClient
    serializerError = SWFSerializer

instance Exception (Er SWF)

-- | The type of the timeout that caused this event.
data ActivityTaskTimeoutType
    = ActivityTaskTimeoutTypeHeartbeat -- ^ HEARTBEAT
    | ActivityTaskTimeoutTypeScheduleToClose -- ^ SCHEDULE_TO_CLOSE
    | ActivityTaskTimeoutTypeScheduleToStart -- ^ SCHEDULE_TO_START
    | ActivityTaskTimeoutTypeStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Show, Generic)

instance Hashable ActivityTaskTimeoutType

instance FromText ActivityTaskTimeoutType where
    parser = match "HEARTBEAT" ActivityTaskTimeoutTypeHeartbeat
         <|> match "SCHEDULE_TO_CLOSE" ActivityTaskTimeoutTypeScheduleToClose
         <|> match "SCHEDULE_TO_START" ActivityTaskTimeoutTypeScheduleToStart
         <|> match "START_TO_CLOSE" ActivityTaskTimeoutTypeStartToClose

instance ToText ActivityTaskTimeoutType where
    toText ActivityTaskTimeoutTypeHeartbeat = "HEARTBEAT"
    toText ActivityTaskTimeoutTypeScheduleToClose = "SCHEDULE_TO_CLOSE"
    toText ActivityTaskTimeoutTypeScheduleToStart = "SCHEDULE_TO_START"
    toText ActivityTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToByteString ActivityTaskTimeoutType where
    toBS ActivityTaskTimeoutTypeHeartbeat = "HEARTBEAT"
    toBS ActivityTaskTimeoutTypeScheduleToClose = "SCHEDULE_TO_CLOSE"
    toBS ActivityTaskTimeoutTypeScheduleToStart = "SCHEDULE_TO_START"
    toBS ActivityTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToHeader ActivityTaskTimeoutType where
    toHeader k = toHeader k . toBS

instance ToQuery ActivityTaskTimeoutType where
    toQuery = toQuery . toBS

instance FromJSON ActivityTaskTimeoutType

instance ToJSON ActivityTaskTimeoutType

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data CancelTimerFailedCause
    = CancelTimerFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CancelTimerFailedCauseTimerIdUnknown -- ^ TIMER_ID_UNKNOWN
      deriving (Eq, Show, Generic)

instance Hashable CancelTimerFailedCause

instance FromText CancelTimerFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" CancelTimerFailedCauseOperationNotPermitted
         <|> match "TIMER_ID_UNKNOWN" CancelTimerFailedCauseTimerIdUnknown

instance ToText CancelTimerFailedCause where
    toText CancelTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText CancelTimerFailedCauseTimerIdUnknown = "TIMER_ID_UNKNOWN"

instance ToByteString CancelTimerFailedCause where
    toBS CancelTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS CancelTimerFailedCauseTimerIdUnknown = "TIMER_ID_UNKNOWN"

instance ToHeader CancelTimerFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery CancelTimerFailedCause where
    toQuery = toQuery . toBS

instance FromJSON CancelTimerFailedCause

instance ToJSON CancelTimerFailedCause

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data CancelWorkflowExecutionFailedCause
    = CancelWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CancelWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
      deriving (Eq, Show, Generic)

instance Hashable CancelWorkflowExecutionFailedCause

instance FromText CancelWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" CancelWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" CancelWorkflowExecutionFailedCauseUnhandledDecision

instance ToText CancelWorkflowExecutionFailedCause where
    toText CancelWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText CancelWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToByteString CancelWorkflowExecutionFailedCause where
    toBS CancelWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS CancelWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToHeader CancelWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery CancelWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON CancelWorkflowExecutionFailedCause

instance ToJSON CancelWorkflowExecutionFailedCause

-- | If set, specifies the default policy to use for the child workflow
-- executions when a workflow execution of this type is terminated, by calling
-- the TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This default can be overridden when starting a workflow execution
-- using the StartWorkflowExecution action or the StartChildWorkflowExecution
-- Decision. The supported child policies are: TERMINATE: the child executions
-- will be terminated. REQUEST_CANCEL: a request to cancel will be attempted
-- for each child execution by recording a WorkflowExecutionCancelRequested
-- event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event. ABANDON: no action
-- will be taken. The child executions will continue to run.
data ChildPolicy
    = ChildPolicyAbandon -- ^ ABANDON
    | ChildPolicyRequestCancel -- ^ REQUEST_CANCEL
    | ChildPolicyTerminate -- ^ TERMINATE
      deriving (Eq, Show, Generic)

instance Hashable ChildPolicy

instance FromText ChildPolicy where
    parser = match "ABANDON" ChildPolicyAbandon
         <|> match "REQUEST_CANCEL" ChildPolicyRequestCancel
         <|> match "TERMINATE" ChildPolicyTerminate

instance ToText ChildPolicy where
    toText ChildPolicyAbandon = "ABANDON"
    toText ChildPolicyRequestCancel = "REQUEST_CANCEL"
    toText ChildPolicyTerminate = "TERMINATE"

instance ToByteString ChildPolicy where
    toBS ChildPolicyAbandon = "ABANDON"
    toBS ChildPolicyRequestCancel = "REQUEST_CANCEL"
    toBS ChildPolicyTerminate = "TERMINATE"

instance ToHeader ChildPolicy where
    toHeader k = toHeader k . toBS

instance ToQuery ChildPolicy where
    toQuery = toQuery . toBS

instance FromJSON ChildPolicy

instance ToJSON ChildPolicy

-- | If the execution status is closed then this specifies how the execution was
-- closed: COMPLETED: the execution was successfully completed. CANCELED: the
-- execution was canceled.Cancellation allows the implementation to gracefully
-- clean up before the execution is closed. TERMINATED: the execution was
-- force terminated. FAILED: the execution failed to complete. TIMED_OUT: the
-- execution did not complete in the alloted time and was automatically timed
-- out. CONTINUED_AS_NEW: the execution is logically continued. This means the
-- current execution was completed and a new execution was started to carry on
-- the workflow.
data CloseStatus
    = CloseStatusCanceled -- ^ CANCELED
    | CloseStatusCompleted -- ^ COMPLETED
    | CloseStatusContinuedAsNew -- ^ CONTINUED_AS_NEW
    | CloseStatusFailed -- ^ FAILED
    | CloseStatusTerminated -- ^ TERMINATED
    | CloseStatusTimedOut -- ^ TIMED_OUT
      deriving (Eq, Show, Generic)

instance Hashable CloseStatus

instance FromText CloseStatus where
    parser = match "CANCELED" CloseStatusCanceled
         <|> match "COMPLETED" CloseStatusCompleted
         <|> match "CONTINUED_AS_NEW" CloseStatusContinuedAsNew
         <|> match "FAILED" CloseStatusFailed
         <|> match "TERMINATED" CloseStatusTerminated
         <|> match "TIMED_OUT" CloseStatusTimedOut

instance ToText CloseStatus where
    toText CloseStatusCanceled = "CANCELED"
    toText CloseStatusCompleted = "COMPLETED"
    toText CloseStatusContinuedAsNew = "CONTINUED_AS_NEW"
    toText CloseStatusFailed = "FAILED"
    toText CloseStatusTerminated = "TERMINATED"
    toText CloseStatusTimedOut = "TIMED_OUT"

instance ToByteString CloseStatus where
    toBS CloseStatusCanceled = "CANCELED"
    toBS CloseStatusCompleted = "COMPLETED"
    toBS CloseStatusContinuedAsNew = "CONTINUED_AS_NEW"
    toBS CloseStatusFailed = "FAILED"
    toBS CloseStatusTerminated = "TERMINATED"
    toBS CloseStatusTimedOut = "TIMED_OUT"

instance ToHeader CloseStatus where
    toHeader k = toHeader k . toBS

instance ToQuery CloseStatus where
    toQuery = toQuery . toBS

instance FromJSON CloseStatus

instance ToJSON CloseStatus

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data CompleteWorkflowExecutionFailedCause
    = CompleteWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CompleteWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
      deriving (Eq, Show, Generic)

instance Hashable CompleteWorkflowExecutionFailedCause

instance FromText CompleteWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" CompleteWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" CompleteWorkflowExecutionFailedCauseUnhandledDecision

instance ToText CompleteWorkflowExecutionFailedCause where
    toText CompleteWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText CompleteWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToByteString CompleteWorkflowExecutionFailedCause where
    toBS CompleteWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS CompleteWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToHeader CompleteWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery CompleteWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON CompleteWorkflowExecutionFailedCause

instance ToJSON CompleteWorkflowExecutionFailedCause

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data ContinueAsNewWorkflowExecutionFailedCause
    = ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined -- ^ DEFAULT_CHILD_POLICY_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined -- ^ DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined -- ^ DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
    | ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated -- ^ WORKFLOW_TYPE_DEPRECATED
    | ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist -- ^ WORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Show, Generic)

instance Hashable ContinueAsNewWorkflowExecutionFailedCause

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    parser = match "DEFAULT_CHILD_POLICY_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined
         <|> match "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined
         <|> match "DEFAULT_TASK_LIST_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined
         <|> match "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined
         <|> match "OPERATION_NOT_PERMITTED" ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision
         <|> match "WORKFLOW_TYPE_DEPRECATED" ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated
         <|> match "WORKFLOW_TYPE_DOES_NOT_EXIST" ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"
    toText ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toText ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToByteString ContinueAsNewWorkflowExecutionFailedCause where
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"
    toBS ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToHeader ContinueAsNewWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery ContinueAsNewWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause

instance ToJSON ContinueAsNewWorkflowExecutionFailedCause

-- | The type of timeout that expired before the decision task could be
-- completed.
data DecisionTaskTimeoutType
    = DecisionTaskTimeoutTypeStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Show, Generic)

instance Hashable DecisionTaskTimeoutType

instance FromText DecisionTaskTimeoutType where
    parser = match "START_TO_CLOSE" DecisionTaskTimeoutTypeStartToClose

instance ToText DecisionTaskTimeoutType where
    toText DecisionTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToByteString DecisionTaskTimeoutType where
    toBS DecisionTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToHeader DecisionTaskTimeoutType where
    toHeader k = toHeader k . toBS

instance ToQuery DecisionTaskTimeoutType where
    toQuery = toQuery . toBS

instance FromJSON DecisionTaskTimeoutType

instance ToJSON DecisionTaskTimeoutType

-- | Specifies the type of the decision.
data DecisionType
    = DecisionTypeCancelTimer -- ^ CancelTimer
    | DecisionTypeCancelWorkflowExecution -- ^ CancelWorkflowExecution
    | DecisionTypeCompleteWorkflowExecution -- ^ CompleteWorkflowExecution
    | DecisionTypeContinueAsNewWorkflowExecution -- ^ ContinueAsNewWorkflowExecution
    | DecisionTypeFailWorkflowExecution -- ^ FailWorkflowExecution
    | DecisionTypeRecordMarker -- ^ RecordMarker
    | DecisionTypeRequestCancelActivityTask -- ^ RequestCancelActivityTask
    | DecisionTypeRequestCancelExternalWorkflowExecution -- ^ RequestCancelExternalWorkflowExecution
    | DecisionTypeScheduleActivityTask -- ^ ScheduleActivityTask
    | DecisionTypeSignalExternalWorkflowExecution -- ^ SignalExternalWorkflowExecution
    | DecisionTypeStartChildWorkflowExecution -- ^ StartChildWorkflowExecution
    | DecisionTypeStartTimer -- ^ StartTimer
      deriving (Eq, Show, Generic)

instance Hashable DecisionType

instance FromText DecisionType where
    parser = match "CancelTimer" DecisionTypeCancelTimer
         <|> match "CancelWorkflowExecution" DecisionTypeCancelWorkflowExecution
         <|> match "CompleteWorkflowExecution" DecisionTypeCompleteWorkflowExecution
         <|> match "ContinueAsNewWorkflowExecution" DecisionTypeContinueAsNewWorkflowExecution
         <|> match "FailWorkflowExecution" DecisionTypeFailWorkflowExecution
         <|> match "RecordMarker" DecisionTypeRecordMarker
         <|> match "RequestCancelActivityTask" DecisionTypeRequestCancelActivityTask
         <|> match "RequestCancelExternalWorkflowExecution" DecisionTypeRequestCancelExternalWorkflowExecution
         <|> match "ScheduleActivityTask" DecisionTypeScheduleActivityTask
         <|> match "SignalExternalWorkflowExecution" DecisionTypeSignalExternalWorkflowExecution
         <|> match "StartChildWorkflowExecution" DecisionTypeStartChildWorkflowExecution
         <|> match "StartTimer" DecisionTypeStartTimer

instance ToText DecisionType where
    toText DecisionTypeCancelTimer = "CancelTimer"
    toText DecisionTypeCancelWorkflowExecution = "CancelWorkflowExecution"
    toText DecisionTypeCompleteWorkflowExecution = "CompleteWorkflowExecution"
    toText DecisionTypeContinueAsNewWorkflowExecution = "ContinueAsNewWorkflowExecution"
    toText DecisionTypeFailWorkflowExecution = "FailWorkflowExecution"
    toText DecisionTypeRecordMarker = "RecordMarker"
    toText DecisionTypeRequestCancelActivityTask = "RequestCancelActivityTask"
    toText DecisionTypeRequestCancelExternalWorkflowExecution = "RequestCancelExternalWorkflowExecution"
    toText DecisionTypeScheduleActivityTask = "ScheduleActivityTask"
    toText DecisionTypeSignalExternalWorkflowExecution = "SignalExternalWorkflowExecution"
    toText DecisionTypeStartChildWorkflowExecution = "StartChildWorkflowExecution"
    toText DecisionTypeStartTimer = "StartTimer"

instance ToByteString DecisionType where
    toBS DecisionTypeCancelTimer = "CancelTimer"
    toBS DecisionTypeCancelWorkflowExecution = "CancelWorkflowExecution"
    toBS DecisionTypeCompleteWorkflowExecution = "CompleteWorkflowExecution"
    toBS DecisionTypeContinueAsNewWorkflowExecution = "ContinueAsNewWorkflowExecution"
    toBS DecisionTypeFailWorkflowExecution = "FailWorkflowExecution"
    toBS DecisionTypeRecordMarker = "RecordMarker"
    toBS DecisionTypeRequestCancelActivityTask = "RequestCancelActivityTask"
    toBS DecisionTypeRequestCancelExternalWorkflowExecution = "RequestCancelExternalWorkflowExecution"
    toBS DecisionTypeScheduleActivityTask = "ScheduleActivityTask"
    toBS DecisionTypeSignalExternalWorkflowExecution = "SignalExternalWorkflowExecution"
    toBS DecisionTypeStartChildWorkflowExecution = "StartChildWorkflowExecution"
    toBS DecisionTypeStartTimer = "StartTimer"

instance ToHeader DecisionType where
    toHeader k = toHeader k . toBS

instance ToQuery DecisionType where
    toQuery = toQuery . toBS

instance FromJSON DecisionType

instance ToJSON DecisionType

-- | The type of the history event.
data EventType
    = EventTypeActivityTaskCancelRequested -- ^ ActivityTaskCancelRequested
    | EventTypeActivityTaskCanceled -- ^ ActivityTaskCanceled
    | EventTypeActivityTaskCompleted -- ^ ActivityTaskCompleted
    | EventTypeActivityTaskFailed -- ^ ActivityTaskFailed
    | EventTypeActivityTaskScheduled -- ^ ActivityTaskScheduled
    | EventTypeActivityTaskStarted -- ^ ActivityTaskStarted
    | EventTypeActivityTaskTimedOut -- ^ ActivityTaskTimedOut
    | EventTypeCancelTimerFailed -- ^ CancelTimerFailed
    | EventTypeCancelWorkflowExecutionFailed -- ^ CancelWorkflowExecutionFailed
    | EventTypeChildWorkflowExecutionCanceled -- ^ ChildWorkflowExecutionCanceled
    | EventTypeChildWorkflowExecutionCompleted -- ^ ChildWorkflowExecutionCompleted
    | EventTypeChildWorkflowExecutionFailed -- ^ ChildWorkflowExecutionFailed
    | EventTypeChildWorkflowExecutionStarted -- ^ ChildWorkflowExecutionStarted
    | EventTypeChildWorkflowExecutionTerminated -- ^ ChildWorkflowExecutionTerminated
    | EventTypeChildWorkflowExecutionTimedOut -- ^ ChildWorkflowExecutionTimedOut
    | EventTypeCompleteWorkflowExecutionFailed -- ^ CompleteWorkflowExecutionFailed
    | EventTypeContinueAsNewWorkflowExecutionFailed -- ^ ContinueAsNewWorkflowExecutionFailed
    | EventTypeDecisionTaskCompleted -- ^ DecisionTaskCompleted
    | EventTypeDecisionTaskScheduled -- ^ DecisionTaskScheduled
    | EventTypeDecisionTaskStarted -- ^ DecisionTaskStarted
    | EventTypeDecisionTaskTimedOut -- ^ DecisionTaskTimedOut
    | EventTypeExternalWorkflowExecutionCancelRequested -- ^ ExternalWorkflowExecutionCancelRequested
    | EventTypeExternalWorkflowExecutionSignaled -- ^ ExternalWorkflowExecutionSignaled
    | EventTypeFailWorkflowExecutionFailed -- ^ FailWorkflowExecutionFailed
    | EventTypeMarkerRecorded -- ^ MarkerRecorded
    | EventTypeRecordMarkerFailed -- ^ RecordMarkerFailed
    | EventTypeRequestCancelActivityTaskFailed -- ^ RequestCancelActivityTaskFailed
    | EventTypeRequestCancelExternalWorkflowExecutionFailed -- ^ RequestCancelExternalWorkflowExecutionFailed
    | EventTypeRequestCancelExternalWorkflowExecutionInitiated -- ^ RequestCancelExternalWorkflowExecutionInitiated
    | EventTypeScheduleActivityTaskFailed -- ^ ScheduleActivityTaskFailed
    | EventTypeSignalExternalWorkflowExecutionFailed -- ^ SignalExternalWorkflowExecutionFailed
    | EventTypeSignalExternalWorkflowExecutionInitiated -- ^ SignalExternalWorkflowExecutionInitiated
    | EventTypeStartChildWorkflowExecutionFailed -- ^ StartChildWorkflowExecutionFailed
    | EventTypeStartChildWorkflowExecutionInitiated -- ^ StartChildWorkflowExecutionInitiated
    | EventTypeStartTimerFailed -- ^ StartTimerFailed
    | EventTypeTimerCanceled -- ^ TimerCanceled
    | EventTypeTimerFired -- ^ TimerFired
    | EventTypeTimerStarted -- ^ TimerStarted
    | EventTypeWorkflowExecutionCancelRequested -- ^ WorkflowExecutionCancelRequested
    | EventTypeWorkflowExecutionCanceled -- ^ WorkflowExecutionCanceled
    | EventTypeWorkflowExecutionCompleted -- ^ WorkflowExecutionCompleted
    | EventTypeWorkflowExecutionContinuedAsNew -- ^ WorkflowExecutionContinuedAsNew
    | EventTypeWorkflowExecutionFailed -- ^ WorkflowExecutionFailed
    | EventTypeWorkflowExecutionSignaled -- ^ WorkflowExecutionSignaled
    | EventTypeWorkflowExecutionStarted -- ^ WorkflowExecutionStarted
    | EventTypeWorkflowExecutionTerminated -- ^ WorkflowExecutionTerminated
    | EventTypeWorkflowExecutionTimedOut -- ^ WorkflowExecutionTimedOut
      deriving (Eq, Show, Generic)

instance Hashable EventType

instance FromText EventType where
    parser = match "ActivityTaskCancelRequested" EventTypeActivityTaskCancelRequested
         <|> match "ActivityTaskCanceled" EventTypeActivityTaskCanceled
         <|> match "ActivityTaskCompleted" EventTypeActivityTaskCompleted
         <|> match "ActivityTaskFailed" EventTypeActivityTaskFailed
         <|> match "ActivityTaskScheduled" EventTypeActivityTaskScheduled
         <|> match "ActivityTaskStarted" EventTypeActivityTaskStarted
         <|> match "ActivityTaskTimedOut" EventTypeActivityTaskTimedOut
         <|> match "CancelTimerFailed" EventTypeCancelTimerFailed
         <|> match "CancelWorkflowExecutionFailed" EventTypeCancelWorkflowExecutionFailed
         <|> match "ChildWorkflowExecutionCanceled" EventTypeChildWorkflowExecutionCanceled
         <|> match "ChildWorkflowExecutionCompleted" EventTypeChildWorkflowExecutionCompleted
         <|> match "ChildWorkflowExecutionFailed" EventTypeChildWorkflowExecutionFailed
         <|> match "ChildWorkflowExecutionStarted" EventTypeChildWorkflowExecutionStarted
         <|> match "ChildWorkflowExecutionTerminated" EventTypeChildWorkflowExecutionTerminated
         <|> match "ChildWorkflowExecutionTimedOut" EventTypeChildWorkflowExecutionTimedOut
         <|> match "CompleteWorkflowExecutionFailed" EventTypeCompleteWorkflowExecutionFailed
         <|> match "ContinueAsNewWorkflowExecutionFailed" EventTypeContinueAsNewWorkflowExecutionFailed
         <|> match "DecisionTaskCompleted" EventTypeDecisionTaskCompleted
         <|> match "DecisionTaskScheduled" EventTypeDecisionTaskScheduled
         <|> match "DecisionTaskStarted" EventTypeDecisionTaskStarted
         <|> match "DecisionTaskTimedOut" EventTypeDecisionTaskTimedOut
         <|> match "ExternalWorkflowExecutionCancelRequested" EventTypeExternalWorkflowExecutionCancelRequested
         <|> match "ExternalWorkflowExecutionSignaled" EventTypeExternalWorkflowExecutionSignaled
         <|> match "FailWorkflowExecutionFailed" EventTypeFailWorkflowExecutionFailed
         <|> match "MarkerRecorded" EventTypeMarkerRecorded
         <|> match "RecordMarkerFailed" EventTypeRecordMarkerFailed
         <|> match "RequestCancelActivityTaskFailed" EventTypeRequestCancelActivityTaskFailed
         <|> match "RequestCancelExternalWorkflowExecutionFailed" EventTypeRequestCancelExternalWorkflowExecutionFailed
         <|> match "RequestCancelExternalWorkflowExecutionInitiated" EventTypeRequestCancelExternalWorkflowExecutionInitiated
         <|> match "ScheduleActivityTaskFailed" EventTypeScheduleActivityTaskFailed
         <|> match "SignalExternalWorkflowExecutionFailed" EventTypeSignalExternalWorkflowExecutionFailed
         <|> match "SignalExternalWorkflowExecutionInitiated" EventTypeSignalExternalWorkflowExecutionInitiated
         <|> match "StartChildWorkflowExecutionFailed" EventTypeStartChildWorkflowExecutionFailed
         <|> match "StartChildWorkflowExecutionInitiated" EventTypeStartChildWorkflowExecutionInitiated
         <|> match "StartTimerFailed" EventTypeStartTimerFailed
         <|> match "TimerCanceled" EventTypeTimerCanceled
         <|> match "TimerFired" EventTypeTimerFired
         <|> match "TimerStarted" EventTypeTimerStarted
         <|> match "WorkflowExecutionCancelRequested" EventTypeWorkflowExecutionCancelRequested
         <|> match "WorkflowExecutionCanceled" EventTypeWorkflowExecutionCanceled
         <|> match "WorkflowExecutionCompleted" EventTypeWorkflowExecutionCompleted
         <|> match "WorkflowExecutionContinuedAsNew" EventTypeWorkflowExecutionContinuedAsNew
         <|> match "WorkflowExecutionFailed" EventTypeWorkflowExecutionFailed
         <|> match "WorkflowExecutionSignaled" EventTypeWorkflowExecutionSignaled
         <|> match "WorkflowExecutionStarted" EventTypeWorkflowExecutionStarted
         <|> match "WorkflowExecutionTerminated" EventTypeWorkflowExecutionTerminated
         <|> match "WorkflowExecutionTimedOut" EventTypeWorkflowExecutionTimedOut

instance ToText EventType where
    toText EventTypeActivityTaskCancelRequested = "ActivityTaskCancelRequested"
    toText EventTypeActivityTaskCanceled = "ActivityTaskCanceled"
    toText EventTypeActivityTaskCompleted = "ActivityTaskCompleted"
    toText EventTypeActivityTaskFailed = "ActivityTaskFailed"
    toText EventTypeActivityTaskScheduled = "ActivityTaskScheduled"
    toText EventTypeActivityTaskStarted = "ActivityTaskStarted"
    toText EventTypeActivityTaskTimedOut = "ActivityTaskTimedOut"
    toText EventTypeCancelTimerFailed = "CancelTimerFailed"
    toText EventTypeCancelWorkflowExecutionFailed = "CancelWorkflowExecutionFailed"
    toText EventTypeChildWorkflowExecutionCanceled = "ChildWorkflowExecutionCanceled"
    toText EventTypeChildWorkflowExecutionCompleted = "ChildWorkflowExecutionCompleted"
    toText EventTypeChildWorkflowExecutionFailed = "ChildWorkflowExecutionFailed"
    toText EventTypeChildWorkflowExecutionStarted = "ChildWorkflowExecutionStarted"
    toText EventTypeChildWorkflowExecutionTerminated = "ChildWorkflowExecutionTerminated"
    toText EventTypeChildWorkflowExecutionTimedOut = "ChildWorkflowExecutionTimedOut"
    toText EventTypeCompleteWorkflowExecutionFailed = "CompleteWorkflowExecutionFailed"
    toText EventTypeContinueAsNewWorkflowExecutionFailed = "ContinueAsNewWorkflowExecutionFailed"
    toText EventTypeDecisionTaskCompleted = "DecisionTaskCompleted"
    toText EventTypeDecisionTaskScheduled = "DecisionTaskScheduled"
    toText EventTypeDecisionTaskStarted = "DecisionTaskStarted"
    toText EventTypeDecisionTaskTimedOut = "DecisionTaskTimedOut"
    toText EventTypeExternalWorkflowExecutionCancelRequested = "ExternalWorkflowExecutionCancelRequested"
    toText EventTypeExternalWorkflowExecutionSignaled = "ExternalWorkflowExecutionSignaled"
    toText EventTypeFailWorkflowExecutionFailed = "FailWorkflowExecutionFailed"
    toText EventTypeMarkerRecorded = "MarkerRecorded"
    toText EventTypeRecordMarkerFailed = "RecordMarkerFailed"
    toText EventTypeRequestCancelActivityTaskFailed = "RequestCancelActivityTaskFailed"
    toText EventTypeRequestCancelExternalWorkflowExecutionFailed = "RequestCancelExternalWorkflowExecutionFailed"
    toText EventTypeRequestCancelExternalWorkflowExecutionInitiated = "RequestCancelExternalWorkflowExecutionInitiated"
    toText EventTypeScheduleActivityTaskFailed = "ScheduleActivityTaskFailed"
    toText EventTypeSignalExternalWorkflowExecutionFailed = "SignalExternalWorkflowExecutionFailed"
    toText EventTypeSignalExternalWorkflowExecutionInitiated = "SignalExternalWorkflowExecutionInitiated"
    toText EventTypeStartChildWorkflowExecutionFailed = "StartChildWorkflowExecutionFailed"
    toText EventTypeStartChildWorkflowExecutionInitiated = "StartChildWorkflowExecutionInitiated"
    toText EventTypeStartTimerFailed = "StartTimerFailed"
    toText EventTypeTimerCanceled = "TimerCanceled"
    toText EventTypeTimerFired = "TimerFired"
    toText EventTypeTimerStarted = "TimerStarted"
    toText EventTypeWorkflowExecutionCancelRequested = "WorkflowExecutionCancelRequested"
    toText EventTypeWorkflowExecutionCanceled = "WorkflowExecutionCanceled"
    toText EventTypeWorkflowExecutionCompleted = "WorkflowExecutionCompleted"
    toText EventTypeWorkflowExecutionContinuedAsNew = "WorkflowExecutionContinuedAsNew"
    toText EventTypeWorkflowExecutionFailed = "WorkflowExecutionFailed"
    toText EventTypeWorkflowExecutionSignaled = "WorkflowExecutionSignaled"
    toText EventTypeWorkflowExecutionStarted = "WorkflowExecutionStarted"
    toText EventTypeWorkflowExecutionTerminated = "WorkflowExecutionTerminated"
    toText EventTypeWorkflowExecutionTimedOut = "WorkflowExecutionTimedOut"

instance ToByteString EventType where
    toBS EventTypeActivityTaskCancelRequested = "ActivityTaskCancelRequested"
    toBS EventTypeActivityTaskCanceled = "ActivityTaskCanceled"
    toBS EventTypeActivityTaskCompleted = "ActivityTaskCompleted"
    toBS EventTypeActivityTaskFailed = "ActivityTaskFailed"
    toBS EventTypeActivityTaskScheduled = "ActivityTaskScheduled"
    toBS EventTypeActivityTaskStarted = "ActivityTaskStarted"
    toBS EventTypeActivityTaskTimedOut = "ActivityTaskTimedOut"
    toBS EventTypeCancelTimerFailed = "CancelTimerFailed"
    toBS EventTypeCancelWorkflowExecutionFailed = "CancelWorkflowExecutionFailed"
    toBS EventTypeChildWorkflowExecutionCanceled = "ChildWorkflowExecutionCanceled"
    toBS EventTypeChildWorkflowExecutionCompleted = "ChildWorkflowExecutionCompleted"
    toBS EventTypeChildWorkflowExecutionFailed = "ChildWorkflowExecutionFailed"
    toBS EventTypeChildWorkflowExecutionStarted = "ChildWorkflowExecutionStarted"
    toBS EventTypeChildWorkflowExecutionTerminated = "ChildWorkflowExecutionTerminated"
    toBS EventTypeChildWorkflowExecutionTimedOut = "ChildWorkflowExecutionTimedOut"
    toBS EventTypeCompleteWorkflowExecutionFailed = "CompleteWorkflowExecutionFailed"
    toBS EventTypeContinueAsNewWorkflowExecutionFailed = "ContinueAsNewWorkflowExecutionFailed"
    toBS EventTypeDecisionTaskCompleted = "DecisionTaskCompleted"
    toBS EventTypeDecisionTaskScheduled = "DecisionTaskScheduled"
    toBS EventTypeDecisionTaskStarted = "DecisionTaskStarted"
    toBS EventTypeDecisionTaskTimedOut = "DecisionTaskTimedOut"
    toBS EventTypeExternalWorkflowExecutionCancelRequested = "ExternalWorkflowExecutionCancelRequested"
    toBS EventTypeExternalWorkflowExecutionSignaled = "ExternalWorkflowExecutionSignaled"
    toBS EventTypeFailWorkflowExecutionFailed = "FailWorkflowExecutionFailed"
    toBS EventTypeMarkerRecorded = "MarkerRecorded"
    toBS EventTypeRecordMarkerFailed = "RecordMarkerFailed"
    toBS EventTypeRequestCancelActivityTaskFailed = "RequestCancelActivityTaskFailed"
    toBS EventTypeRequestCancelExternalWorkflowExecutionFailed = "RequestCancelExternalWorkflowExecutionFailed"
    toBS EventTypeRequestCancelExternalWorkflowExecutionInitiated = "RequestCancelExternalWorkflowExecutionInitiated"
    toBS EventTypeScheduleActivityTaskFailed = "ScheduleActivityTaskFailed"
    toBS EventTypeSignalExternalWorkflowExecutionFailed = "SignalExternalWorkflowExecutionFailed"
    toBS EventTypeSignalExternalWorkflowExecutionInitiated = "SignalExternalWorkflowExecutionInitiated"
    toBS EventTypeStartChildWorkflowExecutionFailed = "StartChildWorkflowExecutionFailed"
    toBS EventTypeStartChildWorkflowExecutionInitiated = "StartChildWorkflowExecutionInitiated"
    toBS EventTypeStartTimerFailed = "StartTimerFailed"
    toBS EventTypeTimerCanceled = "TimerCanceled"
    toBS EventTypeTimerFired = "TimerFired"
    toBS EventTypeTimerStarted = "TimerStarted"
    toBS EventTypeWorkflowExecutionCancelRequested = "WorkflowExecutionCancelRequested"
    toBS EventTypeWorkflowExecutionCanceled = "WorkflowExecutionCanceled"
    toBS EventTypeWorkflowExecutionCompleted = "WorkflowExecutionCompleted"
    toBS EventTypeWorkflowExecutionContinuedAsNew = "WorkflowExecutionContinuedAsNew"
    toBS EventTypeWorkflowExecutionFailed = "WorkflowExecutionFailed"
    toBS EventTypeWorkflowExecutionSignaled = "WorkflowExecutionSignaled"
    toBS EventTypeWorkflowExecutionStarted = "WorkflowExecutionStarted"
    toBS EventTypeWorkflowExecutionTerminated = "WorkflowExecutionTerminated"
    toBS EventTypeWorkflowExecutionTimedOut = "WorkflowExecutionTimedOut"

instance ToHeader EventType where
    toHeader k = toHeader k . toBS

instance ToQuery EventType where
    toQuery = toQuery . toBS

instance FromJSON EventType

instance ToJSON EventType

-- | The current status of the execution.
data ExecutionStatus
    = ExecutionStatusClosed -- ^ CLOSED
    | ExecutionStatusOpen -- ^ OPEN
      deriving (Eq, Show, Generic)

instance Hashable ExecutionStatus

instance FromText ExecutionStatus where
    parser = match "CLOSED" ExecutionStatusClosed
         <|> match "OPEN" ExecutionStatusOpen

instance ToText ExecutionStatus where
    toText ExecutionStatusClosed = "CLOSED"
    toText ExecutionStatusOpen = "OPEN"

instance ToByteString ExecutionStatus where
    toBS ExecutionStatusClosed = "CLOSED"
    toBS ExecutionStatusOpen = "OPEN"

instance ToHeader ExecutionStatus where
    toHeader k = toHeader k . toBS

instance ToQuery ExecutionStatus where
    toQuery = toQuery . toBS

instance FromJSON ExecutionStatus

instance ToJSON ExecutionStatus

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data FailWorkflowExecutionFailedCause
    = FailWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | FailWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
      deriving (Eq, Show, Generic)

instance Hashable FailWorkflowExecutionFailedCause

instance FromText FailWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" FailWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" FailWorkflowExecutionFailedCauseUnhandledDecision

instance ToText FailWorkflowExecutionFailedCause where
    toText FailWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText FailWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToByteString FailWorkflowExecutionFailedCause where
    toBS FailWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS FailWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToHeader FailWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery FailWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON FailWorkflowExecutionFailedCause

instance ToJSON FailWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data RecordMarkerFailedCause
    = RecordMarkerFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Show, Generic)

instance Hashable RecordMarkerFailedCause

instance FromText RecordMarkerFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" RecordMarkerFailedCauseOperationNotPermitted

instance ToText RecordMarkerFailedCause where
    toText RecordMarkerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString RecordMarkerFailedCause where
    toBS RecordMarkerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToHeader RecordMarkerFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery RecordMarkerFailedCause where
    toQuery = toQuery . toBS

instance FromJSON RecordMarkerFailedCause

instance ToJSON RecordMarkerFailedCause

-- | Specifies the registration status of the activity types to list.
data RegistrationStatus
    = RegistrationStatusDeprecated -- ^ DEPRECATED
    | RegistrationStatusRegistered -- ^ REGISTERED
      deriving (Eq, Show, Generic)

instance Hashable RegistrationStatus

instance FromText RegistrationStatus where
    parser = match "DEPRECATED" RegistrationStatusDeprecated
         <|> match "REGISTERED" RegistrationStatusRegistered

instance ToText RegistrationStatus where
    toText RegistrationStatusDeprecated = "DEPRECATED"
    toText RegistrationStatusRegistered = "REGISTERED"

instance ToByteString RegistrationStatus where
    toBS RegistrationStatusDeprecated = "DEPRECATED"
    toBS RegistrationStatusRegistered = "REGISTERED"

instance ToHeader RegistrationStatus where
    toHeader k = toHeader k . toBS

instance ToQuery RegistrationStatus where
    toQuery = toQuery . toBS

instance FromJSON RegistrationStatus

instance ToJSON RegistrationStatus

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data RequestCancelActivityTaskFailedCause
    = RequestCancelActivityTaskFailedCauseActivityIdUnknown -- ^ ACTIVITY_ID_UNKNOWN
    | RequestCancelActivityTaskFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Show, Generic)

instance Hashable RequestCancelActivityTaskFailedCause

instance FromText RequestCancelActivityTaskFailedCause where
    parser = match "ACTIVITY_ID_UNKNOWN" RequestCancelActivityTaskFailedCauseActivityIdUnknown
         <|> match "OPERATION_NOT_PERMITTED" RequestCancelActivityTaskFailedCauseOperationNotPermitted

instance ToText RequestCancelActivityTaskFailedCause where
    toText RequestCancelActivityTaskFailedCauseActivityIdUnknown = "ACTIVITY_ID_UNKNOWN"
    toText RequestCancelActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString RequestCancelActivityTaskFailedCause where
    toBS RequestCancelActivityTaskFailedCauseActivityIdUnknown = "ACTIVITY_ID_UNKNOWN"
    toBS RequestCancelActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToHeader RequestCancelActivityTaskFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery RequestCancelActivityTaskFailedCause where
    toQuery = toQuery . toBS

instance FromJSON RequestCancelActivityTaskFailedCause

instance ToJSON RequestCancelActivityTaskFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data RequestCancelExternalWorkflowExecutionFailedCause
    = RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded -- ^ REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution -- ^ UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Show, Generic)

instance Hashable RequestCancelExternalWorkflowExecutionFailedCause

instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded
         <|> match "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
    toText RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded = "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toText RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToByteString RequestCancelExternalWorkflowExecutionFailedCause where
    toBS RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded = "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toBS RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToHeader RequestCancelExternalWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery RequestCancelExternalWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause

instance ToJSON RequestCancelExternalWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data ScheduleActivityTaskFailedCause
    = ScheduleActivityTaskFailedCauseActivityCreationRateExceeded -- ^ ACTIVITY_CREATION_RATE_EXCEEDED
    | ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse -- ^ ACTIVITY_ID_ALREADY_IN_USE
    | ScheduleActivityTaskFailedCauseActivityTypeDeprecated -- ^ ACTIVITY_TYPE_DEPRECATED
    | ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist -- ^ ACTIVITY_TYPE_DOES_NOT_EXIST
    | ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined -- ^ DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined -- ^ DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined -- ^ DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined -- ^ DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultTaskListUndefined -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded -- ^ OPEN_ACTIVITIES_LIMIT_EXCEEDED
    | ScheduleActivityTaskFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Show, Generic)

instance Hashable ScheduleActivityTaskFailedCause

instance FromText ScheduleActivityTaskFailedCause where
    parser = match "ACTIVITY_CREATION_RATE_EXCEEDED" ScheduleActivityTaskFailedCauseActivityCreationRateExceeded
         <|> match "ACTIVITY_ID_ALREADY_IN_USE" ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse
         <|> match "ACTIVITY_TYPE_DEPRECATED" ScheduleActivityTaskFailedCauseActivityTypeDeprecated
         <|> match "ACTIVITY_TYPE_DOES_NOT_EXIST" ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist
         <|> match "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined
         <|> match "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined
         <|> match "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined
         <|> match "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined
         <|> match "DEFAULT_TASK_LIST_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultTaskListUndefined
         <|> match "OPEN_ACTIVITIES_LIMIT_EXCEEDED" ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded
         <|> match "OPERATION_NOT_PERMITTED" ScheduleActivityTaskFailedCauseOperationNotPermitted

instance ToText ScheduleActivityTaskFailedCause where
    toText ScheduleActivityTaskFailedCauseActivityCreationRateExceeded = "ACTIVITY_CREATION_RATE_EXCEEDED"
    toText ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse = "ACTIVITY_ID_ALREADY_IN_USE"
    toText ScheduleActivityTaskFailedCauseActivityTypeDeprecated = "ACTIVITY_TYPE_DEPRECATED"
    toText ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist = "ACTIVITY_TYPE_DOES_NOT_EXIST"
    toText ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined = "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined = "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined = "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined = "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded = "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    toText ScheduleActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString ScheduleActivityTaskFailedCause where
    toBS ScheduleActivityTaskFailedCauseActivityCreationRateExceeded = "ACTIVITY_CREATION_RATE_EXCEEDED"
    toBS ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse = "ACTIVITY_ID_ALREADY_IN_USE"
    toBS ScheduleActivityTaskFailedCauseActivityTypeDeprecated = "ACTIVITY_TYPE_DEPRECATED"
    toBS ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist = "ACTIVITY_TYPE_DOES_NOT_EXIST"
    toBS ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined = "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined = "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined = "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined = "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded = "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    toBS ScheduleActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToHeader ScheduleActivityTaskFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery ScheduleActivityTaskFailedCause where
    toQuery = toQuery . toBS

instance FromJSON ScheduleActivityTaskFailedCause

instance ToJSON ScheduleActivityTaskFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data SignalExternalWorkflowExecutionFailedCause
    = SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded -- ^ SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution -- ^ UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Show, Generic)

instance Hashable SignalExternalWorkflowExecutionFailedCause

instance FromText SignalExternalWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded
         <|> match "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded = "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toText SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToByteString SignalExternalWorkflowExecutionFailedCause where
    toBS SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded = "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toBS SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToHeader SignalExternalWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery SignalExternalWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON SignalExternalWorkflowExecutionFailedCause

instance ToJSON SignalExternalWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data StartChildWorkflowExecutionFailedCause
    = StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded -- ^ CHILD_CREATION_RATE_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined -- ^ DEFAULT_CHILD_POLICY_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined -- ^ DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined -- ^ DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded -- ^ OPEN_CHILDREN_LIMIT_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded -- ^ OPEN_WORKFLOWS_LIMIT_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning -- ^ WORKFLOW_ALREADY_RUNNING
    | StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated -- ^ WORKFLOW_TYPE_DEPRECATED
    | StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist -- ^ WORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Show, Generic)

instance Hashable StartChildWorkflowExecutionFailedCause

instance FromText StartChildWorkflowExecutionFailedCause where
    parser = match "CHILD_CREATION_RATE_EXCEEDED" StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded
         <|> match "DEFAULT_CHILD_POLICY_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined
         <|> match "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined
         <|> match "DEFAULT_TASK_LIST_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined
         <|> match "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined
         <|> match "OPEN_CHILDREN_LIMIT_EXCEEDED" StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded
         <|> match "OPEN_WORKFLOWS_LIMIT_EXCEEDED" StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded
         <|> match "OPERATION_NOT_PERMITTED" StartChildWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "WORKFLOW_ALREADY_RUNNING" StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning
         <|> match "WORKFLOW_TYPE_DEPRECATED" StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated
         <|> match "WORKFLOW_TYPE_DOES_NOT_EXIST" StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist

instance ToText StartChildWorkflowExecutionFailedCause where
    toText StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded = "CHILD_CREATION_RATE_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded = "OPEN_CHILDREN_LIMIT_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded = "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning = "WORKFLOW_ALREADY_RUNNING"
    toText StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toText StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToByteString StartChildWorkflowExecutionFailedCause where
    toBS StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded = "CHILD_CREATION_RATE_EXCEEDED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded = "OPEN_CHILDREN_LIMIT_EXCEEDED"
    toBS StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded = "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    toBS StartChildWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning = "WORKFLOW_ALREADY_RUNNING"
    toBS StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toBS StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToHeader StartChildWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery StartChildWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON StartChildWorkflowExecutionFailedCause

instance ToJSON StartChildWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data StartTimerFailedCause
    = StartTimerFailedCauseOpenTimersLimitExceeded -- ^ OPEN_TIMERS_LIMIT_EXCEEDED
    | StartTimerFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | StartTimerFailedCauseTimerCreationRateExceeded -- ^ TIMER_CREATION_RATE_EXCEEDED
    | StartTimerFailedCauseTimerIdAlreadyInUse -- ^ TIMER_ID_ALREADY_IN_USE
      deriving (Eq, Show, Generic)

instance Hashable StartTimerFailedCause

instance FromText StartTimerFailedCause where
    parser = match "OPEN_TIMERS_LIMIT_EXCEEDED" StartTimerFailedCauseOpenTimersLimitExceeded
         <|> match "OPERATION_NOT_PERMITTED" StartTimerFailedCauseOperationNotPermitted
         <|> match "TIMER_CREATION_RATE_EXCEEDED" StartTimerFailedCauseTimerCreationRateExceeded
         <|> match "TIMER_ID_ALREADY_IN_USE" StartTimerFailedCauseTimerIdAlreadyInUse

instance ToText StartTimerFailedCause where
    toText StartTimerFailedCauseOpenTimersLimitExceeded = "OPEN_TIMERS_LIMIT_EXCEEDED"
    toText StartTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText StartTimerFailedCauseTimerCreationRateExceeded = "TIMER_CREATION_RATE_EXCEEDED"
    toText StartTimerFailedCauseTimerIdAlreadyInUse = "TIMER_ID_ALREADY_IN_USE"

instance ToByteString StartTimerFailedCause where
    toBS StartTimerFailedCauseOpenTimersLimitExceeded = "OPEN_TIMERS_LIMIT_EXCEEDED"
    toBS StartTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS StartTimerFailedCauseTimerCreationRateExceeded = "TIMER_CREATION_RATE_EXCEEDED"
    toBS StartTimerFailedCauseTimerIdAlreadyInUse = "TIMER_ID_ALREADY_IN_USE"

instance ToHeader StartTimerFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery StartTimerFailedCause where
    toQuery = toQuery . toBS

instance FromJSON StartTimerFailedCause

instance ToJSON StartTimerFailedCause

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child policy
-- is set to cancel child executions.
data WorkflowExecutionCancelRequestedCause
    = WorkflowExecutionCancelRequestedCauseChildPolicyApplied -- ^ CHILD_POLICY_APPLIED
      deriving (Eq, Show, Generic)

instance Hashable WorkflowExecutionCancelRequestedCause

instance FromText WorkflowExecutionCancelRequestedCause where
    parser = match "CHILD_POLICY_APPLIED" WorkflowExecutionCancelRequestedCauseChildPolicyApplied

instance ToText WorkflowExecutionCancelRequestedCause where
    toText WorkflowExecutionCancelRequestedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"

instance ToByteString WorkflowExecutionCancelRequestedCause where
    toBS WorkflowExecutionCancelRequestedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"

instance ToHeader WorkflowExecutionCancelRequestedCause where
    toHeader k = toHeader k . toBS

instance ToQuery WorkflowExecutionCancelRequestedCause where
    toQuery = toQuery . toBS

instance FromJSON WorkflowExecutionCancelRequestedCause

instance ToJSON WorkflowExecutionCancelRequestedCause

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution
-- times out or is terminated and the child policy is set to terminate child
-- executions.
data WorkflowExecutionTerminatedCause
    = WorkflowExecutionTerminatedCauseChildPolicyApplied -- ^ CHILD_POLICY_APPLIED
    | WorkflowExecutionTerminatedCauseEventLimitExceeded -- ^ EVENT_LIMIT_EXCEEDED
    | WorkflowExecutionTerminatedCauseOperatorInitiated -- ^ OPERATOR_INITIATED
      deriving (Eq, Show, Generic)

instance Hashable WorkflowExecutionTerminatedCause

instance FromText WorkflowExecutionTerminatedCause where
    parser = match "CHILD_POLICY_APPLIED" WorkflowExecutionTerminatedCauseChildPolicyApplied
         <|> match "EVENT_LIMIT_EXCEEDED" WorkflowExecutionTerminatedCauseEventLimitExceeded
         <|> match "OPERATOR_INITIATED" WorkflowExecutionTerminatedCauseOperatorInitiated

instance ToText WorkflowExecutionTerminatedCause where
    toText WorkflowExecutionTerminatedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"
    toText WorkflowExecutionTerminatedCauseEventLimitExceeded = "EVENT_LIMIT_EXCEEDED"
    toText WorkflowExecutionTerminatedCauseOperatorInitiated = "OPERATOR_INITIATED"

instance ToByteString WorkflowExecutionTerminatedCause where
    toBS WorkflowExecutionTerminatedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"
    toBS WorkflowExecutionTerminatedCauseEventLimitExceeded = "EVENT_LIMIT_EXCEEDED"
    toBS WorkflowExecutionTerminatedCauseOperatorInitiated = "OPERATOR_INITIATED"

instance ToHeader WorkflowExecutionTerminatedCause where
    toHeader k = toHeader k . toBS

instance ToQuery WorkflowExecutionTerminatedCause where
    toQuery = toQuery . toBS

instance FromJSON WorkflowExecutionTerminatedCause

instance ToJSON WorkflowExecutionTerminatedCause

-- | The type of the timeout that caused the child workflow execution to time
-- out.
data WorkflowExecutionTimeoutType
    = WorkflowExecutionTimeoutTypeStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Show, Generic)

instance Hashable WorkflowExecutionTimeoutType

instance FromText WorkflowExecutionTimeoutType where
    parser = match "START_TO_CLOSE" WorkflowExecutionTimeoutTypeStartToClose

instance ToText WorkflowExecutionTimeoutType where
    toText WorkflowExecutionTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToByteString WorkflowExecutionTimeoutType where
    toBS WorkflowExecutionTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToHeader WorkflowExecutionTimeoutType where
    toHeader k = toHeader k . toBS

instance ToQuery WorkflowExecutionTimeoutType where
    toQuery = toQuery . toBS

instance FromJSON WorkflowExecutionTimeoutType

instance ToJSON WorkflowExecutionTimeoutType

-- | Provides details of the CancelTimer decision. It is not set for other
-- decision types.
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes
    { _ctdaTimerId :: Text
      -- ^ The unique Id of the timer to cancel. This field is required.
    } deriving (Show, Generic)

instance FromJSON CancelTimerDecisionAttributes

instance ToJSON CancelTimerDecisionAttributes

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { _cwedbDetails :: Maybe Text
      -- ^ Optional details of the cancellation.
    } deriving (Show, Generic)

instance FromJSON CancelWorkflowExecutionDecisionAttributes

instance ToJSON CancelWorkflowExecutionDecisionAttributes

-- | If specified, only workflow executions that match this close status are
-- listed. For example, if TERMINATED is specified, then only TERMINATED
-- workflow executions are listed. closeStatusFilter, executionFilter,
-- typeFilter and tagFilter are mutually exclusive. You can specify at most
-- one of these in a request.
newtype CloseStatusFilter = CloseStatusFilter
    { _csfStatus :: CloseStatus
      -- ^ The close status that must match the close status of an execution
      -- for it to meet the criteria of this filter. This field is
      -- required.
    } deriving (Show, Generic)

instance ToJSON CloseStatusFilter

-- | Provides details of the CompleteWorkflowExecution decision. It is not set
-- for other decision types.
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { _cwedaResult :: Maybe Text
      -- ^ The result of the workflow execution. The form of the result is
      -- implementation defined.
    } deriving (Show, Generic)

instance FromJSON CompleteWorkflowExecutionDecisionAttributes

instance ToJSON CompleteWorkflowExecutionDecisionAttributes

-- | Contains the configuration settings of a domain.
newtype DomainConfiguration = DomainConfiguration
    { _ddpWorkflowExecutionRetentionPeriodInDays :: Text
      -- ^ The retention period for workflow executions in this domain.
    } deriving (Show, Generic)

instance FromJSON DomainConfiguration

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes
    { _rcatdaActivityId :: Text
      -- ^ The activityId of the activity task to be canceled.
    } deriving (Show, Generic)

instance FromJSON RequestCancelActivityTaskDecisionAttributes

instance ToJSON RequestCancelActivityTaskDecisionAttributes

-- | If specified, only executions that have the matching tag are listed.
-- executionFilter, typeFilter and tagFilter are mutually exclusive. You can
-- specify at most one of these in a request.
newtype TagFilter = TagFilter
    { _tfTag :: Text
      -- ^ Specifies the tag that must be associated with the execution for
      -- it to meet the filter criteria. This field is required.
    } deriving (Show, Generic)

instance ToJSON TagFilter

-- | If set, specifies the default task list to use for scheduling tasks of this
-- activity type. This default task list is used if a task list is not
-- provided when a task is scheduled through the ScheduleActivityTask
-- Decision.
newtype TaskList = TaskList
    { _tmName :: Text
      -- ^ The name of the task list.
    } deriving (Show, Generic)

instance FromJSON TaskList

instance ToJSON TaskList

-- | If specified, only workflow executions matching the workflow id specified
-- in the filter are returned. executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
newtype WorkflowExecutionFilter = WorkflowExecutionFilter
    { _wefWorkflowId :: Text
      -- ^ The workflowId to pass of match the criteria of this filter.
    } deriving (Show, Generic)

instance ToJSON WorkflowExecutionFilter

-- | If the event is of type ActivityTaskcancelRequested then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes
    { _atcreaActivityId :: Text
      -- ^ The unique ID of the task.
    , _atcreaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RequestCancelActivityTask
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskCancelRequestedEventAttributes

instance ToJSON ActivityTaskCancelRequestedEventAttributes

-- | If the event is of type ActivityTaskCanceled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes
    { _atcebLatestCancelRequestedEventId :: Maybe Integer
      -- ^ If set, contains the Id of the last ActivityTaskCancelRequested
      -- event recorded for this activity task. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _atcebScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atcebDetails :: Maybe Text
      -- ^ Details of the cancellation (if any).
    , _atcebStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskCanceledEventAttributes

instance ToJSON ActivityTaskCanceledEventAttributes

-- | If the event is of type ActivityTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes
    { _atceaScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atceaResult :: Maybe Text
      -- ^ The results of the activity task (if any).
    , _atceaStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskCompletedEventAttributes

instance ToJSON ActivityTaskCompletedEventAttributes

-- | If the event is of type ActivityTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes
    { _atfeaScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atfeaReason :: Maybe Text
      -- ^ The reason provided for the failure (if any).
    , _atfeaDetails :: Maybe Text
      -- ^ The details of the failure (if any).
    , _atfeaStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskFailedEventAttributes

instance ToJSON ActivityTaskFailedEventAttributes

-- | If the event is of type ActivityTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes
    { _atseaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks. This data is not sent to
      -- the activity.
    , _atseaActivityType :: ActivityType
      -- ^ The type of the activity task.
    , _atseaActivityId :: Text
      -- ^ The unique id of the activity task.
    , _atseaHeartbeatTimeout :: Maybe Text
      -- ^ The maximum time before which the worker processing this task
      -- must report progress by calling RecordActivityTaskHeartbeat. If
      -- the timeout is exceeded, the activity task is automatically timed
      -- out. If the worker subsequently attempts to record a heartbeat or
      -- return a result, it will be ignored.
    , _atseaScheduleToCloseTimeout :: Maybe Text
      -- ^ The maximum amount of time for this activity task.
    , _atseaInput :: Maybe Text
      -- ^ The input provided to the activity task.
    , _atseaTaskList :: TaskList
      -- ^ The task list in which the activity task has been scheduled.
    , _atseaScheduleToStartTimeout :: Maybe Text
      -- ^ The maximum amount of time the activity task can wait to be
      -- assigned to a worker.
    , _atseaStartToCloseTimeout :: Maybe Text
      -- ^ The maximum amount of time a worker may take to process the
      -- activity task.
    , _atseaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision that resulted in the scheduling of this activity task.
      -- This information can be useful for diagnosing problems by tracing
      -- back the chain of events leading up to this event.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskScheduledEventAttributes

instance ToJSON ActivityTaskScheduledEventAttributes

-- | If the event is of type ActivityTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes
    { _atsebScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atsebIdentity :: Maybe Text
      -- ^ Identity of the worker that was assigned this task. This aids
      -- diagnostics when problems arise. The form of this identity is
      -- user defined.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskStartedEventAttributes

instance ToJSON ActivityTaskStartedEventAttributes

-- | If the event is of type ActivityTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes
    { _attoeaScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _attoeaTimeoutType :: ActivityTaskTimeoutType
      -- ^ The type of the timeout that caused this event.
    , _attoeaDetails :: Maybe Text
      -- ^ Contains the content of the details parameter for the last call
      -- made by the activity to RecordActivityTaskHeartbeat.
    , _attoeaStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

instance FromJSON ActivityTaskTimedOutEventAttributes

instance ToJSON ActivityTaskTimedOutEventAttributes

-- | The ActivityType type structure representing the activity type.
data ActivityType = ActivityType
    { _atName :: Text
      -- ^ The name of this activity. The combination of activity type name
      -- and version must be unique within a domain.
    , _atVersion :: Text
      -- ^ The version of this activity. The combination of activity type
      -- name and version must be unique with in a domain.
    } deriving (Show, Generic)

instance FromJSON ActivityType

instance ToJSON ActivityType

-- | The configuration settings registered with the activity type.
data ActivityTypeConfiguration = ActivityTypeConfiguration
    { _atcDefaultTaskScheduleToStartTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the activity type, that a task of an activity type can wait
      -- before being assigned to a worker. You can override this default
      -- when scheduling a task through the ScheduleActivityTask Decision.
      -- The valid values are integers greater than or equal to 0. An
      -- integer value can be used to specify the duration in seconds
      -- while NONE can be used to specify unlimited duration.
    , _atcDefaultTaskList :: Maybe TaskList
      -- ^ The optional default task list specified for this activity type
      -- at registration. This default task list is used if a task list is
      -- not provided when a task is scheduled through the
      -- ScheduleActivityTask Decision. You can override this default when
      -- scheduling a task through the ScheduleActivityTask Decision.
    , _atcDefaultTaskHeartbeatTimeout :: Maybe Text
      -- ^ The optional default maximum time, specified when registering the
      -- activity type, before which a worker processing a task must
      -- report progress by calling RecordActivityTaskHeartbeat. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision. If the activity worker
      -- subsequently attempts to record a heartbeat or returns a result,
      -- the activity worker receives an UnknownResource fault. In this
      -- case, Amazon SWF no longer considers the activity task to be
      -- valid; the activity worker should clean up the activity task. The
      -- valid values are integers greater than or equal to 0. An integer
      -- value can be used to specify the duration in seconds while NONE
      -- can be used to specify unlimited duration.
    , _atcDefaultTaskScheduleToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the activity type, for tasks of this activity type. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    , _atcDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration for tasks of an activity
      -- type specified when registering the activity type. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    } deriving (Show, Generic)

instance FromJSON ActivityTypeConfiguration

-- | Detailed information about an activity type.
data ActivityTypeInfo = ActivityTypeInfo
    { _atjStatus :: RegistrationStatus
      -- ^ The current status of the activity type.
    , _atjActivityType :: ActivityType
      -- ^ The ActivityType type structure representing the activity type.
    , _atjDeprecationDate :: Maybe POSIX
      -- ^ If DEPRECATED, the date and time DeprecateActivityType was
      -- called.
    , _atjCreationDate :: POSIX
      -- ^ The date and time this activity type was created through
      -- RegisterActivityType.
    , _atjDescription :: Maybe Text
      -- ^ The description of the activity type provided in
      -- RegisterActivityType.
    } deriving (Show, Generic)

instance FromJSON ActivityTypeInfo

-- | If the event is of type CancelTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes
    { _ctfeaCause :: CancelTimerFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _ctfeaTimerId :: Text
      -- ^ The timerId provided in the CancelTimer decision that failed.
    , _ctfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelTimer decision to cancel
      -- this timer. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON CancelTimerFailedEventAttributes

instance ToJSON CancelTimerFailedEventAttributes

-- | If the event is of type CancelWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes
    { _cwefebCause :: CancelWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _cwefebDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelWorkflowExecution
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON CancelWorkflowExecutionFailedEventAttributes

instance ToJSON CancelWorkflowExecutionFailedEventAttributes

-- | If the event is of type ChildWorkflowExecutionCanceled then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes
    { _cwecebWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cwecebDetails :: Maybe Text
      -- ^ Details of the cancellation (if provided).
    , _cwecebStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwecebInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwecebWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was canceled.
    } deriving (Show, Generic)

instance FromJSON ChildWorkflowExecutionCanceledEventAttributes

instance ToJSON ChildWorkflowExecutionCanceledEventAttributes

-- | If the event is of type ChildWorkflowExecutionCompleted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes
    { _cweceaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cweceaResult :: Maybe Text
      -- ^ The result of the child workflow execution (if any).
    , _cweceaStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweceaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweceaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was completed.
    } deriving (Show, Generic)

instance FromJSON ChildWorkflowExecutionCompletedEventAttributes

instance ToJSON ChildWorkflowExecutionCompletedEventAttributes

-- | If the event is of type ChildWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes
    { _cwefecWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cwefecReason :: Maybe Text
      -- ^ The reason for the failure (if provided).
    , _cwefecDetails :: Maybe Text
      -- ^ The details of the failure (if provided).
    , _cwefecStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwefecInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwefecWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that failed.
    } deriving (Show, Generic)

instance FromJSON ChildWorkflowExecutionFailedEventAttributes

instance ToJSON ChildWorkflowExecutionFailedEventAttributes

-- | If the event is of type ChildWorkflowExecutionStarted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes
    { _cweseaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cweseaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweseaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was started.
    } deriving (Show, Generic)

instance FromJSON ChildWorkflowExecutionStartedEventAttributes

instance ToJSON ChildWorkflowExecutionStartedEventAttributes

-- | If the event is of type ChildWorkflowExecutionTerminated then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes
    { _cweteaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cweteaStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweteaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweteaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was terminated.
    } deriving (Show, Generic)

instance FromJSON ChildWorkflowExecutionTerminatedEventAttributes

instance ToJSON ChildWorkflowExecutionTerminatedEventAttributes

-- | If the event is of type ChildWorkflowExecutionTimedOut then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes
    { _cwetoeaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cwetoeaTimeoutType :: WorkflowExecutionTimeoutType
      -- ^ The type of the timeout that caused the child workflow execution
      -- to time out.
    , _cwetoeaStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwetoeaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwetoeaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that timed out.
    } deriving (Show, Generic)

instance FromJSON ChildWorkflowExecutionTimedOutEventAttributes

instance ToJSON ChildWorkflowExecutionTimedOutEventAttributes

-- | If the event is of type CompleteWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes
    { _cwefeaCause :: CompleteWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _cwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CompleteWorkflowExecution
      -- decision to complete this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON CompleteWorkflowExecutionFailedEventAttributes

instance ToJSON CompleteWorkflowExecutionFailedEventAttributes

-- | Provides details of the ContinueAsNewWorkflowExecution decision. It is not
-- set for other decision types.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { _canwedaTagList :: [Text]
      -- ^ The list of tags to associate with the new workflow execution. A
      -- maximum of 5 tags can be specified. You can list workflow
      -- executions with a specific tag by calling
      -- ListOpenWorkflowExecutions or ListClosedWorkflowExecutions and
      -- specifying a TagFilter.
    , _canwedaTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for the new
      -- workflow execution. This parameter overrides the
      -- defaultTaskStartToCloseTimout specified when registering the
      -- workflow type using RegisterWorkflowType. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for the
      -- new workflow execution must be specified either as a default for
      -- the workflow type or through this parameter. If neither this
      -- parameter is set nor a default task start-to-close timeout was
      -- specified at registration time then a fault will be returned.
    , _canwedaInput :: Maybe Text
      -- ^ The input provided to the new workflow execution.
    , _canwedaWorkflowTypeVersion :: Maybe Text
    , _canwedaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the total duration for this workflow execution.
      -- This overrides the defaultExecutionStartToCloseTimeout specified
      -- when registering the workflow type. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. An execution start-to-close timeout for this
      -- workflow execution must be specified either as a default for the
      -- workflow type or through this field. If neither this field is set
      -- nor a default execution start-to-close timeout was specified at
      -- registration time then a fault will be returned.
    , _canwedaTaskList :: Maybe TaskList
      -- ^ Represents a task list.
    , _canwedaChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow
      -- executions of the new execution if it is terminated by calling
      -- the TerminateWorkflowExecution action explicitly or due to an
      -- expired timeout. This policy overrides the default child policy
      -- specified when registering the workflow type using
      -- RegisterWorkflowType. The supported child policies are:
      -- TERMINATE: the child executions will be terminated.
      -- REQUEST_CANCEL: a request to cancel will be attempted for each
      -- child execution by recording a WorkflowExecutionCancelRequested
      -- event in its history. It is up to the decider to take appropriate
      -- actions when it receives an execution history with this event.
      -- ABANDON: no action will be taken. The child executions will
      -- continue to run. A child policy for the new workflow execution
      -- must be specified either as a default registered for its workflow
      -- type or through this field. If neither this field is set nor a
      -- default child policy was specified at registration time then a
      -- fault will be returned.
    } deriving (Show, Generic)

instance FromJSON ContinueAsNewWorkflowExecutionDecisionAttributes

instance ToJSON ContinueAsNewWorkflowExecutionDecisionAttributes

-- | If the event is of type ContinueAsNewWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { _canwefeaCause :: ContinueAsNewWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _canwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the ContinueAsNewWorkflowExecution
      -- decision that started this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON ContinueAsNewWorkflowExecutionFailedEventAttributes

instance ToJSON ContinueAsNewWorkflowExecutionFailedEventAttributes

-- | Specifies a decision made by the decider. A decision can be one of these
-- types: CancelTimer cancels a previously started timer and records a
-- TimerCanceled event in the history. CancelWorkflowExecution closes the
-- workflow execution and records a WorkflowExecutionCanceled event in the
-- history. CompleteWorkflowExecution closes the workflow execution and
-- records a WorkflowExecutionCompleted event in the history .
-- ContinueAsNewWorkflowExecution closes the workflow execution and starts a
-- new workflow execution of the same type using the same workflow id and a
-- unique run Id. A WorkflowExecutionContinuedAsNew event is recorded in the
-- history. FailWorkflowExecution closes the workflow execution and records a
-- WorkflowExecutionFailed event in the history. RecordMarker records a
-- MarkerRecorded event in the history. Markers can be used for adding custom
-- information in the history for instance to let deciders know that they do
-- not need to look at the history beyond the marker event.
-- RequestCancelActivityTask attempts to cancel a previously scheduled
-- activity task. If the activity task was scheduled but has not been assigned
-- to a worker, then it will be canceled. If the activity task was already
-- assigned to a worker, then the worker will be informed that cancellation
-- has been requested in the response to RecordActivityTaskHeartbeat.
-- RequestCancelExternalWorkflowExecution requests that a request be made to
-- cancel the specified external workflow execution and records a
-- RequestCancelExternalWorkflowExecutionInitiated event in the history.
-- ScheduleActivityTask schedules an activity task.
-- SignalExternalWorkflowExecution requests a signal to be delivered to the
-- specified external workflow execution and records a
-- SignalExternalWorkflowExecutionInitiated event in the history.
-- StartChildWorkflowExecution requests that a child workflow execution be
-- started and records a StartChildWorkflowExecutionInitiated event in the
-- history. The child workflow execution is a separate workflow execution with
-- its own history. StartTimer starts a timer for this workflow execution and
-- records a TimerStarted event in the history. This timer will fire after the
-- specified delay and record a TimerFired event. Access Control If you grant
-- permission to use RespondDecisionTaskCompleted, you can use IAM policies to
-- express permissions for the list of decisions returned by this action as if
-- they were members of the API. Treating decisions as a pseudo API maintains
-- a uniform conceptual model and helps keep policies readable. For details
-- and example IAM policies, see Using IAM to Manage Access to Amazon SWF
-- Workflows. Decision Failure Decisions can fail for several reasons The
-- ordering of decisions should follow a logical flow. Some decisions might
-- not make sense in the current context of the workflow execution and will
-- therefore fail. A limit on your account was reached. The decision lacks
-- sufficient permissions. One of the following events might be added to the
-- history to indicate an error. The event attribute's cause parameter
-- indicates the cause. If cause is set to OPERATION_NOT_PERMITTED, the
-- decision failed because it lacked sufficient permissions.
-- ScheduleActivityTaskFailed a ScheduleActivityTask decision failed. This
-- could happen if the activity type specified in the decision is not
-- registered, is in a deprecated state, or the decision is not properly
-- configured. RequestCancelActivityTaskFailed a RequestCancelActivityTask
-- decision failed. This could happen if there is no open activity task with
-- the specified activityId. StartTimerFailed a StartTimer decision failed.
-- This could happen if there is another open timer with the same timerId.
-- CancelTimerFailed a CancelTimer decision failed. This could happen if there
-- is no open timer with the specified timerId.
-- StartChildWorkflowExecutionFailed a StartChildWorkflowExecution decision
-- failed. This could happen if the workflow type specified is not registered,
-- is deprecated, or the decision is not properly configured.
-- SignalExternalWorkflowExecutionFailed a SignalExternalWorkflowExecution
-- decision failed. This could happen if the workflowID specified in the
-- decision was incorrect. RequestCancelExternalWorkflowExecutionFailed a
-- RequestCancelExternalWorkflowExecution decision failed. This could happen
-- if the workflowID specified in the decision was incorrect.
-- CancelWorkflowExecutionFailed a CancelWorkflowExecution decision failed.
-- This could happen if there is an unhandled decision task pending in the
-- workflow execution. CompleteWorkflowExecutionFailed a
-- CompleteWorkflowExecution decision failed. This could happen if there is an
-- unhandled decision task pending in the workflow execution.
-- ContinueAsNewWorkflowExecutionFailed a ContinueAsNewWorkflowExecution
-- decision failed. This could happen if there is an unhandled decision task
-- pending in the workflow execution or the ContinueAsNewWorkflowExecution
-- decision was not configured correctly. FailWorkflowExecutionFailed a
-- FailWorkflowExecution decision failed. This could happen if there is an
-- unhandled decision task pending in the workflow execution. The preceding
-- error events might occur due to an error in the decider logic, which might
-- put the workflow execution in an unstable state The cause field in the
-- event structure for the error event indicates the cause of the error. A
-- workflow execution may be closed by the decider by returning one of the
-- following decisions when completing a decision task:
-- CompleteWorkflowExecution, FailWorkflowExecution, CancelWorkflowExecution
-- and ContinueAsNewWorkflowExecution. An UnhandledDecision fault will be
-- returned if a workflow closing decision is specified and a signal or
-- activity event had been added to the history while the decision task was
-- being performed by the decider. Unlike the above situations which are logic
-- issues, this fault is always possible because of race conditions in a
-- distributed system. The right action here is to call
-- RespondDecisionTaskCompleted without any decisions. This would result in
-- another decision task with these new events included in the history. The
-- decider should handle the new events and may decide to close the workflow
-- execution. How to Code a Decision You code a decision by first setting the
-- decision type field to one of the above decision values, and then set the
-- corresponding attributes field shown below:
-- ScheduleActivityTaskDecisionAttributes
-- RequestCancelActivityTaskDecisionAttributes
-- CompleteWorkflowExecutionDecisionAttributes
-- FailWorkflowExecutionDecisionAttributes
-- CancelWorkflowExecutionDecisionAttributes
-- ContinueAsNewWorkflowExecutionDecisionAttributes
-- RecordMarkerDecisionAttributes StartTimerDecisionAttributes
-- CancelTimerDecisionAttributes
-- SignalExternalWorkflowExecutionDecisionAttributes
-- RequestCancelExternalWorkflowExecutionDecisionAttributes
-- StartChildWorkflowExecutionDecisionAttributes.
data Decision = Decision
    { _dzRequestCancelExternalWorkflowExecutionDecisionAttributes :: Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the RequestCancelExternalWorkflowExecution
      -- decision. It is not set for other decision types.
    , _dzScheduleActivityTaskDecisionAttributes :: Maybe ScheduleActivityTaskDecisionAttributes
      -- ^ Provides details of the ScheduleActivityTask decision. It is not
      -- set for other decision types.
    , _dzSignalExternalWorkflowExecutionDecisionAttributes :: Maybe SignalExternalWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the SignalExternalWorkflowExecution decision.
      -- It is not set for other decision types.
    , _dzStartTimerDecisionAttributes :: Maybe StartTimerDecisionAttributes
      -- ^ Provides details of the StartTimer decision. It is not set for
      -- other decision types.
    , _dzDecisionType :: DecisionType
      -- ^ Specifies the type of the decision.
    , _dzRecordMarkerDecisionAttributes :: Maybe RecordMarkerDecisionAttributes
      -- ^ Provides details of the RecordMarker decision. It is not set for
      -- other decision types.
    , _dzFailWorkflowExecutionDecisionAttributes :: Maybe FailWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the FailWorkflowExecution decision. It is not
      -- set for other decision types.
    , _dzStartChildWorkflowExecutionDecisionAttributes :: Maybe StartChildWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the StartChildWorkflowExecution decision. It
      -- is not set for other decision types.
    , _dzCompleteWorkflowExecutionDecisionAttributes :: Maybe CompleteWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the CompleteWorkflowExecution decision. It is
      -- not set for other decision types.
    , _dzRequestCancelActivityTaskDecisionAttributes :: Maybe RequestCancelActivityTaskDecisionAttributes
      -- ^ Provides details of the RequestCancelActivityTask decision. It is
      -- not set for other decision types.
    , _dzCancelWorkflowExecutionDecisionAttributes :: Maybe CancelWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the CancelWorkflowExecution decision. It is
      -- not set for other decision types.
    , _dzCancelTimerDecisionAttributes :: Maybe CancelTimerDecisionAttributes
      -- ^ Provides details of the CancelTimer decision. It is not set for
      -- other decision types.
    , _dzContinueAsNewWorkflowExecutionDecisionAttributes :: Maybe ContinueAsNewWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the ContinueAsNewWorkflowExecution decision.
      -- It is not set for other decision types.
    } deriving (Show, Generic)

instance ToJSON Decision

-- | If the event is of type DecisionTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes
    { _dtceaScheduledEventId :: Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when
      -- this decision task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _dtceaExecutionContext :: Maybe Text
      -- ^ User defined context for the workflow execution.
    , _dtceaStartedEventId :: Integer
      -- ^ The Id of the DecisionTaskStarted event recorded when this
      -- decision task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

instance FromJSON DecisionTaskCompletedEventAttributes

instance ToJSON DecisionTaskCompletedEventAttributes

-- | If the event is of type DecisionTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes
    { _dtseaTaskList :: TaskList
      -- ^ The name of the task list in which the decision task was
      -- scheduled.
    , _dtseaStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this decision task. The task is
      -- considered timed out if it does not completed within this
      -- duration. The valid values are integers greater than or equal to
      -- 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    } deriving (Show, Generic)

instance FromJSON DecisionTaskScheduledEventAttributes

instance ToJSON DecisionTaskScheduledEventAttributes

-- | If the event is of type DecisionTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes
    { _dtsebScheduledEventId :: Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when
      -- this decision task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _dtsebIdentity :: Maybe Text
      -- ^ Identity of the decider making the request. This enables
      -- diagnostic tracing when problems arise. The form of this identity
      -- is user defined.
    } deriving (Show, Generic)

instance FromJSON DecisionTaskStartedEventAttributes

instance ToJSON DecisionTaskStartedEventAttributes

-- | If the event is of type DecisionTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes
    { _dttoeaScheduledEventId :: Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when
      -- this decision task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _dttoeaTimeoutType :: DecisionTaskTimeoutType
      -- ^ The type of timeout that expired before the decision task could
      -- be completed.
    , _dttoeaStartedEventId :: Integer
      -- ^ The Id of the DecisionTaskStarted event recorded when this
      -- decision task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

instance FromJSON DecisionTaskTimedOutEventAttributes

instance ToJSON DecisionTaskTimedOutEventAttributes

-- | Contains general information about a domain.
data DomainInfo = DomainInfo
    { _ddqStatus :: RegistrationStatus
      -- ^ The status of the domain: REGISTERED: The domain is properly
      -- registered and available. You can use this domain for registering
      -- types and creating new workflow executions. DEPRECATED: The
      -- domain was deprecated using DeprecateDomain, but is still in use.
      -- You should not create new workflow executions in this domain.
    , _ddqName :: Text
      -- ^ The name of the domain. This name is unique within the account.
    , _ddqDescription :: Maybe Text
      -- ^ The description of the domain provided through RegisterDomain.
    } deriving (Show, Generic)

instance FromJSON DomainInfo

-- | Workflow executions are included in the returned results based on whether
-- their start times are within the range specified by this filter.
data ExecutionTimeFilter = ExecutionTimeFilter
    { _etfLatestDate :: Maybe POSIX
      -- ^ Specifies the latest start or close date and time to return.
    , _etfOldestDate :: POSIX
      -- ^ Specifies the oldest start or close date and time to return.
    } deriving (Show, Generic)

instance ToJSON ExecutionTimeFilter

-- | If the event is of type ExternalWorkflowExecutionCancelRequested then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { _ewecreaInitiatedEventId :: Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated
      -- event corresponding to the RequestCancelExternalWorkflowExecution
      -- decision to cancel this external workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event.
    , _ewecreaWorkflowExecution :: WorkflowExecution
      -- ^ The external workflow execution to which the cancellation request
      -- was delivered.
    } deriving (Show, Generic)

instance FromJSON ExternalWorkflowExecutionCancelRequestedEventAttributes

instance ToJSON ExternalWorkflowExecutionCancelRequestedEventAttributes

-- | If the event is of type ExternalWorkflowExecutionSignaled then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes
    { _eweseaInitiatedEventId :: Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event
      -- corresponding to the SignalExternalWorkflowExecution decision to
      -- request this signal. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    , _eweseaWorkflowExecution :: WorkflowExecution
      -- ^ The external workflow execution that the signal was delivered to.
    } deriving (Show, Generic)

instance FromJSON ExternalWorkflowExecutionSignaledEventAttributes

instance ToJSON ExternalWorkflowExecutionSignaledEventAttributes

-- | Provides details of the FailWorkflowExecution decision. It is not set for
-- other decision types.
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { _fwedaReason :: Maybe Text
      -- ^ A descriptive reason for the failure that may help in
      -- diagnostics.
    , _fwedaDetails :: Maybe Text
      -- ^ Optional details of the failure.
    } deriving (Show, Generic)

instance FromJSON FailWorkflowExecutionDecisionAttributes

instance ToJSON FailWorkflowExecutionDecisionAttributes

-- | If the event is of type FailWorkflowExecutionFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes
    { _fwefeaCause :: FailWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _fwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the FailWorkflowExecution decision
      -- to fail this execution. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON FailWorkflowExecutionFailedEventAttributes

instance ToJSON FailWorkflowExecutionFailedEventAttributes

-- | Event within a workflow execution. A history event can be one of these
-- types: WorkflowExecutionStarted: The workflow execution was started.
-- WorkflowExecutionCompleted: The workflow execution was closed due to
-- successful completion. WorkflowExecutionFailed: The workflow execution
-- closed due to a failure. WorkflowExecutionTimedOut: The workflow execution
-- was closed because a time out was exceeded. WorkflowExecutionCanceled: The
-- workflow execution was successfully canceled and closed.
-- WorkflowExecutionTerminated: The workflow execution was terminated.
-- WorkflowExecutionContinuedAsNew: The workflow execution was closed and a
-- new execution of the same type was created with the same workflowId.
-- WorkflowExecutionCancelRequested: A request to cancel this workflow
-- execution was made. DecisionTaskScheduled: A decision task was scheduled
-- for the workflow execution. DecisionTaskStarted: The decision task was
-- dispatched to a decider. DecisionTaskCompleted: The decider successfully
-- completed a decision task by calling RespondDecisionTaskCompleted.
-- DecisionTaskFailed: The decider failed a decision task by calling
-- RespondDecisionTaskFailed. --> DecisionTaskTimedOut: The decision task
-- timed out. ActivityTaskScheduled: An activity task was scheduled for
-- execution. ScheduleActivityTaskFailed: Failed to process
-- ScheduleActivityTask decision. This happens when the decision is not
-- configured properly, for example the activity type specified is not
-- registered. ActivityTaskStarted: The scheduled activity task was dispatched
-- to a worker. ActivityTaskCompleted: An activity worker successfully
-- completed an activity task by calling RespondActivityTaskCompleted.
-- ActivityTaskFailed: An activity worker failed an activity task by calling
-- RespondActivityTaskFailed. ActivityTaskTimedOut: The activity task timed
-- out. ActivityTaskCanceled: The activity task was successfully canceled.
-- ActivityTaskHeartbeatRecorded: A call to RecordActivityTaskHeartbeat was
-- successfully processed by the system. --> ActivityTaskCancelRequested: A
-- RequestCancelActivityTask decision was received by the system.
-- RequestCancelActivityTaskFailed: Failed to process
-- RequestCancelActivityTask decision. This happens when the decision is not
-- configured properly. WorkflowExecutionSignaled: An external signal was
-- received for the workflow execution. MarkerRecorded: A marker was recorded
-- in the workflow history as the result of a RecordMarker decision.
-- TimerStarted: A timer was started for the workflow execution due to a
-- StartTimer decision. StartTimerFailed: Failed to process StartTimer
-- decision. This happens when the decision is not configured properly, for
-- example a timer already exists with the specified timer Id. TimerFired: A
-- timer, previously started for this workflow execution, fired.
-- TimerCanceled: A timer, previously started for this workflow execution, was
-- successfully canceled. CancelTimerFailed: Failed to process CancelTimer
-- decision. This happens when the decision is not configured properly, for
-- example no timer exists with the specified timer Id.
-- StartChildWorkflowExecutionInitiated: A request was made to start a child
-- workflow execution. StartChildWorkflowExecutionFailed: Failed to process
-- StartChildWorkflowExecution decision. This happens when the decision is not
-- configured properly, for example the workflow type specified is not
-- registered. ChildWorkflowExecutionStarted: A child workflow execution was
-- successfully started. ChildWorkflowExecutionCompleted: A child workflow
-- execution, started by this workflow execution, completed successfully and
-- was closed. ChildWorkflowExecutionFailed: A child workflow execution,
-- started by this workflow execution, failed to complete successfully and was
-- closed. ChildWorkflowExecutionTimedOut: A child workflow execution, started
-- by this workflow execution, timed out and was closed.
-- ChildWorkflowExecutionCanceled: A child workflow execution, started by this
-- workflow execution, was canceled and closed.
-- ChildWorkflowExecutionTerminated: A child workflow execution, started by
-- this workflow execution, was terminated.
-- SignalExternalWorkflowExecutionInitiated: A request to signal an external
-- workflow was made. ExternalWorkflowExecutionSignaled: A signal, requested
-- by this workflow execution, was successfully delivered to the target
-- external workflow execution. SignalExternalWorkflowExecutionFailed: The
-- request to signal an external workflow execution failed.
-- RequestCancelExternalWorkflowExecutionInitiated: A request was made to
-- request the cancellation of an external workflow execution.
-- ExternalWorkflowExecutionCancelRequested: Request to cancel an external
-- workflow execution was successfully delivered to the target execution.
-- RequestCancelExternalWorkflowExecutionFailed: Request to cancel an external
-- workflow execution failed.
data HistoryEvent = HistoryEvent
    { _heWorkflowExecutionCancelRequestedEventAttributes :: Maybe WorkflowExecutionCancelRequestedEventAttributes
      -- ^ If the event is of type WorkflowExecutionCancelRequested then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heRecordMarkerFailedEventAttributes :: Maybe RecordMarkerFailedEventAttributes
      -- ^ If the event is of type DecisionTaskFailed then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type
      -- RequestCancelExternalWorkflowExecutionInitiated then this member
      -- is set and provides detailed information about the event. It is
      -- not set for other event types.
    , _heDecisionTaskScheduledEventAttributes :: Maybe DecisionTaskScheduledEventAttributes
      -- ^ If the event is of type DecisionTaskScheduled then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heWorkflowExecutionCompletedEventAttributes :: Maybe WorkflowExecutionCompletedEventAttributes
      -- ^ If the event is of type WorkflowExecutionCompleted then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heStartTimerFailedEventAttributes :: Maybe StartTimerFailedEventAttributes
      -- ^ If the event is of type StartTimerFailed then this member is set
      -- and provides detailed information about the event. It is not set
      -- for other event types.
    , _heEventTimestamp :: POSIX
      -- ^ The date and time when the event occurred.
    , _heActivityTaskScheduledEventAttributes :: Maybe ActivityTaskScheduledEventAttributes
      -- ^ If the event is of type ActivityTaskScheduled then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heScheduleActivityTaskFailedEventAttributes :: Maybe ScheduleActivityTaskFailedEventAttributes
      -- ^ If the event is of type ScheduleActivityTaskFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionCompletedEventAttributes :: Maybe ChildWorkflowExecutionCompletedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionCompleted then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heMarkerRecordedEventAttributes :: Maybe MarkerRecordedEventAttributes
      -- ^ If the event is of type MarkerRecorded then this member is set
      -- and provides detailed information about the event. It is not set
      -- for other event types.
    , _heCompleteWorkflowExecutionFailedEventAttributes :: Maybe CompleteWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type CompleteWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type
      -- RequestCancelExternalWorkflowExecutionFailed then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heTimerCanceledEventAttributes :: Maybe TimerCanceledEventAttributes
      -- ^ If the event is of type TimerCanceled then this member is set and
      -- provides detailed information about the event. It is not set for
      -- other event types.
    , _heWorkflowExecutionStartedEventAttributes :: Maybe WorkflowExecutionStartedEventAttributes
      -- ^ If the event is of type WorkflowExecutionStarted then this member
      -- is set and provides detailed information about the event. It is
      -- not set for other event types.
    , _heActivityTaskCompletedEventAttributes :: Maybe ActivityTaskCompletedEventAttributes
      -- ^ If the event is of type ActivityTaskCompleted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heDecisionTaskTimedOutEventAttributes :: Maybe DecisionTaskTimedOutEventAttributes
      -- ^ If the event is of type DecisionTaskTimedOut then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heCancelTimerFailedEventAttributes :: Maybe CancelTimerFailedEventAttributes
      -- ^ If the event is of type CancelTimerFailed then this member is set
      -- and provides detailed information about the event. It is not set
      -- for other event types.
    , _heChildWorkflowExecutionStartedEventAttributes :: Maybe ChildWorkflowExecutionStartedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionStarted then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heEventType :: EventType
      -- ^ The type of the history event.
    , _heActivityTaskCanceledEventAttributes :: Maybe ActivityTaskCanceledEventAttributes
      -- ^ If the event is of type ActivityTaskCanceled then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskTimedOutEventAttributes :: Maybe ActivityTaskTimedOutEventAttributes
      -- ^ If the event is of type ActivityTaskTimedOut then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heDecisionTaskStartedEventAttributes :: Maybe DecisionTaskStartedEventAttributes
      -- ^ If the event is of type DecisionTaskStarted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heWorkflowExecutionTerminatedEventAttributes :: Maybe WorkflowExecutionTerminatedEventAttributes
      -- ^ If the event is of type WorkflowExecutionTerminated then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionCanceledEventAttributes :: Maybe ChildWorkflowExecutionCanceledEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionCanceled then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heRequestCancelActivityTaskFailedEventAttributes :: Maybe RequestCancelActivityTaskFailedEventAttributes
      -- ^ If the event is of type RequestCancelActivityTaskFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionTimedOutEventAttributes :: Maybe ChildWorkflowExecutionTimedOutEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionTimedOut then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heCancelWorkflowExecutionFailedEventAttributes :: Maybe CancelWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type CancelWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heStartChildWorkflowExecutionInitiatedEventAttributes :: Maybe StartChildWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type StartChildWorkflowExecutionInitiated then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heSignalExternalWorkflowExecutionFailedEventAttributes :: Maybe SignalExternalWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type SignalExternalWorkflowExecutionFailed
      -- then this member is set and provides detailed information about
      -- the event. It is not set for other event types.
    , _heActivityTaskStartedEventAttributes :: Maybe ActivityTaskStartedEventAttributes
      -- ^ If the event is of type ActivityTaskStarted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heChildWorkflowExecutionTerminatedEventAttributes :: Maybe ChildWorkflowExecutionTerminatedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionTerminated then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heWorkflowExecutionCanceledEventAttributes :: Maybe WorkflowExecutionCanceledEventAttributes
      -- ^ If the event is of type WorkflowExecutionCanceled then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heTimerStartedEventAttributes :: Maybe TimerStartedEventAttributes
      -- ^ If the event is of type TimerStarted then this member is set and
      -- provides detailed information about the event. It is not set for
      -- other event types.
    , _heActivityTaskCancelRequestedEventAttributes :: Maybe ActivityTaskCancelRequestedEventAttributes
      -- ^ If the event is of type ActivityTaskcancelRequested then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionTimedOutEventAttributes :: Maybe WorkflowExecutionTimedOutEventAttributes
      -- ^ If the event is of type WorkflowExecutionTimedOut then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionSignaledEventAttributes :: Maybe WorkflowExecutionSignaledEventAttributes
      -- ^ If the event is of type WorkflowExecutionSignaled then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heTimerFiredEventAttributes :: Maybe TimerFiredEventAttributes
      -- ^ If the event is of type TimerFired then this member is set and
      -- provides detailed information about the event. It is not set for
      -- other event types.
    , _heActivityTaskFailedEventAttributes :: Maybe ActivityTaskFailedEventAttributes
      -- ^ If the event is of type ActivityTaskFailed then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heExternalWorkflowExecutionSignaledEventAttributes :: Maybe ExternalWorkflowExecutionSignaledEventAttributes
      -- ^ If the event is of type ExternalWorkflowExecutionSignaled then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heDecisionTaskCompletedEventAttributes :: Maybe DecisionTaskCompletedEventAttributes
      -- ^ If the event is of type DecisionTaskCompleted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heStartChildWorkflowExecutionFailedEventAttributes :: Maybe StartChildWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type StartChildWorkflowExecutionFailed then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heChildWorkflowExecutionFailedEventAttributes :: Maybe ChildWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heFailWorkflowExecutionFailedEventAttributes :: Maybe FailWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type FailWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heContinueAsNewWorkflowExecutionFailedEventAttributes :: Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type ContinueAsNewWorkflowExecutionFailed then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type SignalExternalWorkflowExecutionInitiated
      -- then this member is set and provides detailed information about
      -- the event. It is not set for other event types.
    , _heEventId :: Integer
      -- ^ The system generated id of the event. This id uniquely identifies
      -- the event with in the workflow execution history.
    , _heWorkflowExecutionFailedEventAttributes :: Maybe WorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type WorkflowExecutionFailed then this member
      -- is set and provides detailed information about the event. It is
      -- not set for other event types.
    , _heWorkflowExecutionContinuedAsNewEventAttributes :: Maybe WorkflowExecutionContinuedAsNewEventAttributes
      -- ^ If the event is of type WorkflowExecutionContinuedAsNew then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heExternalWorkflowExecutionCancelRequestedEventAttributes :: Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
      -- ^ If the event is of type ExternalWorkflowExecutionCancelRequested
      -- then this member is set and provides detailed information about
      -- the event. It is not set for other event types.
    } deriving (Show, Generic)

instance FromJSON HistoryEvent

-- | If the event is of type MarkerRecorded then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes
    { _mreaMarkerName :: Text
      -- ^ The name of the marker.
    , _mreaDetails :: Maybe Text
      -- ^ Details of the marker (if any).
    , _mreaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RecordMarker decision that
      -- requested this marker. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON MarkerRecordedEventAttributes

instance ToJSON MarkerRecordedEventAttributes

-- | Provides details of the RecordMarker decision. It is not set for other
-- decision types.
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes
    { _rmdaMarkerName :: Text
      -- ^ The name of the marker. This file is required.
    , _rmdaDetails :: Maybe Text
      -- ^ Optional details of the marker.
    } deriving (Show, Generic)

instance FromJSON RecordMarkerDecisionAttributes

instance ToJSON RecordMarkerDecisionAttributes

-- | If the event is of type DecisionTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes
    { _rmfeaMarkerName :: Text
      -- ^ The marker's name.
    , _rmfeaCause :: RecordMarkerFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _rmfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RecordMarkerFailed decision
      -- for this cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON RecordMarkerFailedEventAttributes

instance ToJSON RecordMarkerFailedEventAttributes

-- | If the event is of type RequestCancelActivityTaskFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes
    { _rcatfeaActivityId :: Text
      -- ^ The activityId provided in the RequestCancelActivityTask decision
      -- that failed.
    , _rcatfeaCause :: RequestCancelActivityTaskFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _rcatfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RequestCancelActivityTask
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON RequestCancelActivityTaskFailedEventAttributes

instance ToJSON RequestCancelActivityTaskFailedEventAttributes

-- | Provides details of the RequestCancelExternalWorkflowExecution decision. It
-- is not set for other decision types.
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { _rcewedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    , _rcewedaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to cancel.
    , _rcewedaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution to cancel. This
      -- field is required.
    } deriving (Show, Generic)

instance FromJSON RequestCancelExternalWorkflowExecutionDecisionAttributes

instance ToJSON RequestCancelExternalWorkflowExecutionDecisionAttributes

-- | If the event is of type RequestCancelExternalWorkflowExecutionFailed then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
data RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { _rcewefeaControl :: Maybe Text
    , _rcewefeaCause :: RequestCancelExternalWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _rcewefeaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution.
    , _rcewefeaInitiatedEventId :: Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated
      -- event corresponding to the RequestCancelExternalWorkflowExecution
      -- decision to cancel this external workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event.
    , _rcewefeaWorkflowId :: Text
      -- ^ The workflowId of the external workflow to which the cancel
      -- request was to be delivered.
    , _rcewefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- RequestCancelExternalWorkflowExecution decision for this
      -- cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes

instance ToJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes

-- | If the event is of type RequestCancelExternalWorkflowExecutionInitiated
-- then this member is set and provides detailed information about the event.
-- It is not set for other event types.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { _rceweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    , _rceweieaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to be canceled.
    , _rceweieaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution to be canceled.
    , _rceweieaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- RequestCancelExternalWorkflowExecution decision for this
      -- cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

instance ToJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

-- | Provides details of the ScheduleActivityTask decision. It is not set for
-- other decision types.
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes
    { _satdaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks. This data is not sent to
      -- the activity.
    , _satdaActivityType :: ActivityType
      -- ^ The type of the activity task to schedule. This field is
      -- required.
    , _satdaActivityId :: Text
      -- ^ The activityId of the activity task. This field is required. The
      -- specified string must not start or end with whitespace. It must
      -- not contain a : (colon), / (slash), | (vertical bar), or any
      -- control characters (\u0000-\u001f | \u007f - \u009f). Also, it
      -- must not contain the literal string &quot;arn&quot;.
    , _satdaHeartbeatTimeout :: Maybe Text
      -- ^ If set, specifies the maximum time before which a worker
      -- processing a task of this type must report progress by calling
      -- RecordActivityTaskHeartbeat. If the timeout is exceeded, the
      -- activity task is automatically timed out. If the worker
      -- subsequently attempts to record a heartbeat or returns a result,
      -- it will be ignored. This overrides the default heartbeat timeout
      -- specified when registering the activity type using
      -- RegisterActivityType. The valid values are integers greater than
      -- or equal to 0. An integer value can be used to specify the
      -- duration in seconds while NONE can be used to specify unlimited
      -- duration.
    , _satdaScheduleToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this activity task. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A schedule-to-close timeout for this
      -- activity task must be specified either as a default for the
      -- activity type or through this field. If neither this field is set
      -- nor a default schedule-to-close timeout was specified at
      -- registration time then a fault will be returned.
    , _satdaInput :: Maybe Text
      -- ^ The input provided to the activity task.
    , _satdaTaskList :: Maybe TaskList
      -- ^ If set, specifies the name of the task list in which to schedule
      -- the activity task. If not specified, the defaultTaskList
      -- registered with the activity type will be used. A task list for
      -- this activity task must be specified either as a default for the
      -- activity type or through this field. If neither this field is set
      -- nor a default task list was specified at registration time then a
      -- fault will be returned. The specified string must not start or
      -- end with whitespace. It must not contain a : (colon), / (slash),
      -- | (vertical bar), or any control characters (\u0000-\u001f |
      -- \u007f - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    , _satdaScheduleToStartTimeout :: Maybe Text
      -- ^ If set, specifies the maximum duration the activity task can wait
      -- to be assigned to a worker. This overrides the default
      -- schedule-to-start timeout specified when registering the activity
      -- type using RegisterActivityType. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. A schedule-to-start timeout for this activity
      -- task must be specified either as a default for the activity type
      -- or through this field. If neither this field is set nor a default
      -- schedule-to-start timeout was specified at registration time then
      -- a fault will be returned.
    , _satdaStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the maximum duration a worker may take to
      -- process this activity task. This overrides the default
      -- start-to-close timeout specified when registering the activity
      -- type using RegisterActivityType. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. A start-to-close timeout for this activity
      -- task must be specified either as a default for the activity type
      -- or through this field. If neither this field is set nor a default
      -- start-to-close timeout was specified at registration time then a
      -- fault will be returned.
    } deriving (Show, Generic)

instance FromJSON ScheduleActivityTaskDecisionAttributes

instance ToJSON ScheduleActivityTaskDecisionAttributes

-- | If the event is of type ScheduleActivityTaskFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes
    { _satfeaActivityType :: ActivityType
      -- ^ The activity type provided in the ScheduleActivityTask decision
      -- that failed.
    , _satfeaActivityId :: Text
      -- ^ The activityId provided in the ScheduleActivityTask decision that
      -- failed.
    , _satfeaCause :: ScheduleActivityTaskFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _satfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision that resulted in the scheduling of this activity task.
      -- This information can be useful for diagnosing problems by tracing
      -- back the chain of events leading up to this event.
    } deriving (Show, Generic)

instance FromJSON ScheduleActivityTaskFailedEventAttributes

instance ToJSON ScheduleActivityTaskFailedEventAttributes

-- | Provides details of the SignalExternalWorkflowExecution decision. It is not
-- set for other decision types.
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes
    { _sewedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent decision tasks.
    , _sewedaInput :: Maybe Text
      -- ^ Optional input to be provided with the signal.The target workflow
      -- execution will use the signal name and input to process the
      -- signal.
    , _sewedaRunId :: Maybe Text
      -- ^ The runId of the workflow execution to be signaled.
    , _sewedaWorkflowId :: Text
      -- ^ The workflowId of the workflow execution to be signaled. This
      -- field is required.
    , _sewedaSignalName :: Text
      -- ^ The name of the signal.The target workflow execution will use the
      -- signal name and input to process the signal. This field is
      -- required.
    } deriving (Show, Generic)

instance FromJSON SignalExternalWorkflowExecutionDecisionAttributes

instance ToJSON SignalExternalWorkflowExecutionDecisionAttributes

-- | If the event is of type SignalExternalWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes
    { _sewefeaControl :: Maybe Text
    , _sewefeaCause :: SignalExternalWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _sewefeaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution that the signal was
      -- being delivered to.
    , _sewefeaInitiatedEventId :: Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event
      -- corresponding to the SignalExternalWorkflowExecution decision to
      -- request this signal. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    , _sewefeaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution that the signal
      -- was being delivered to.
    , _sewefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- SignalExternalWorkflowExecution decision for this signal. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events leading up to this event.
    } deriving (Show, Generic)

instance FromJSON SignalExternalWorkflowExecutionFailedEventAttributes

instance ToJSON SignalExternalWorkflowExecutionFailedEventAttributes

-- | If the event is of type SignalExternalWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { _seweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent decision tasks.
    , _seweieaInput :: Maybe Text
      -- ^ Input provided to the signal (if any).
    , _seweieaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to send the signal
      -- to.
    , _seweieaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution.
    , _seweieaSignalName :: Text
      -- ^ The name of the signal.
    , _seweieaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- SignalExternalWorkflowExecution decision for this signal. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events leading up to this event.
    } deriving (Show, Generic)

instance FromJSON SignalExternalWorkflowExecutionInitiatedEventAttributes

instance ToJSON SignalExternalWorkflowExecutionInitiatedEventAttributes

-- | Provides details of the StartChildWorkflowExecution decision. It is not set
-- for other decision types.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes
    { _scwedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks. This data is not sent to
      -- the child workflow execution.
    , _scwedaTagList :: [Text]
      -- ^ The list of tags to associate with the child workflow execution.
      -- A maximum of 5 tags can be specified. You can list workflow
      -- executions with a specific tag by calling
      -- ListOpenWorkflowExecutions or ListClosedWorkflowExecutions and
      -- specifying a TagFilter.
    , _scwedaTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for this
      -- workflow execution. This parameter overrides the
      -- defaultTaskStartToCloseTimout specified when registering the
      -- workflow type using RegisterWorkflowType. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for
      -- this workflow execution must be specified either as a default for
      -- the workflow type or through this parameter. If neither this
      -- parameter is set nor a default task start-to-close timeout was
      -- specified at registration time then a fault will be returned.
    , _scwedaWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution to be started. This field is
      -- required.
    , _scwedaInput :: Maybe Text
      -- ^ The input to be provided to the workflow execution.
    , _scwedaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration for this workflow execution. This overrides
      -- the defaultExecutionStartToCloseTimeout specified when
      -- registering the workflow type. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. An execution start-to-close timeout for this
      -- workflow execution must be specified either as a default for the
      -- workflow type or through this parameter. If neither this
      -- parameter is set nor a default execution start-to-close timeout
      -- was specified at registration time then a fault will be returned.
    , _scwedaTaskList :: Maybe TaskList
      -- ^ The name of the task list to be used for decision tasks of the
      -- child workflow execution. A task list for this workflow execution
      -- must be specified either as a default for the workflow type or
      -- through this parameter. If neither this parameter is set nor a
      -- default task list was specified at registration time then a fault
      -- will be returned. The specified string must not start or end with
      -- whitespace. It must not contain a : (colon), / (slash), |
      -- (vertical bar), or any control characters (\u0000-\u001f | \u007f
      -- - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    , _scwedaChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow
      -- executions if the workflow execution being started is terminated
      -- by calling the TerminateWorkflowExecution action explicitly or
      -- due to an expired timeout. This policy overrides the default
      -- child policy specified when registering the workflow type using
      -- RegisterWorkflowType. The supported child policies are:
      -- TERMINATE: the child executions will be terminated.
      -- REQUEST_CANCEL: a request to cancel will be attempted for each
      -- child execution by recording a WorkflowExecutionCancelRequested
      -- event in its history. It is up to the decider to take appropriate
      -- actions when it receives an execution history with this event.
      -- ABANDON: no action will be taken. The child executions will
      -- continue to run. A child policy for the workflow execution being
      -- started must be specified either as a default registered for its
      -- workflow type or through this field. If neither this field is set
      -- nor a default child policy was specified at registration time
      -- then a fault will be returned.
    , _scwedaWorkflowId :: Text
      -- ^ The workflowId of the workflow execution. This field is required.
      -- The specified string must not start or end with whitespace. It
      -- must not contain a : (colon), / (slash), | (vertical bar), or any
      -- control characters (\u0000-\u001f | \u007f - \u009f). Also, it
      -- must not contain the literal string &quot;arn&quot;.
    } deriving (Show, Generic)

instance FromJSON StartChildWorkflowExecutionDecisionAttributes

instance ToJSON StartChildWorkflowExecutionDecisionAttributes

-- | If the event is of type StartChildWorkflowExecutionFailed then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes
    { _scwefeaControl :: Maybe Text
    , _scwefeaWorkflowType :: WorkflowType
      -- ^ The workflow type provided in the StartChildWorkflowExecution
      -- Decision that failed.
    , _scwefeaCause :: StartChildWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _scwefeaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _scwefeaWorkflowId :: Text
      -- ^ The workflowId of the child workflow execution.
    , _scwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartChildWorkflowExecution
      -- Decision to request this child workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events.
    } deriving (Show, Generic)

instance FromJSON StartChildWorkflowExecutionFailedEventAttributes

instance ToJSON StartChildWorkflowExecutionFailedEventAttributes

-- | If the event is of type StartChildWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes
    { _scweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent decision tasks. This data is not sent to
      -- the activity.
    , _scweieaTagList :: [Text]
      -- ^ The list of tags to associated with the child workflow execution.
    , _scweieaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration allowed for the decision tasks for this
      -- workflow execution. The valid values are integers greater than or
      -- equal to 0. An integer value can be used to specify the duration
      -- in seconds while NONE can be used to specify unlimited duration.
    , _scweieaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _scweieaInput :: Maybe Text
      -- ^ The inputs provided to the child workflow execution (if any).
    , _scweieaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for the child workflow execution. If the
      -- workflow execution is not closed within this duration, it will be
      -- timed out and force terminated. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    , _scweieaTaskList :: TaskList
      -- ^ The name of the task list used for the decision tasks of the
      -- child workflow execution.
    , _scweieaChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions if this
      -- execution gets terminated by explicitly calling the
      -- TerminateWorkflowExecution action or due to an expired timeout.
      -- The supported child policies are: TERMINATE: the child executions
      -- will be terminated. REQUEST_CANCEL: a request to cancel will be
      -- attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _scweieaWorkflowId :: Text
      -- ^ The workflowId of the child workflow execution.
    , _scweieaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartChildWorkflowExecution
      -- Decision to request this child workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events.
    } deriving (Show, Generic)

instance FromJSON StartChildWorkflowExecutionInitiatedEventAttributes

instance ToJSON StartChildWorkflowExecutionInitiatedEventAttributes

-- | Provides details of the StartTimer decision. It is not set for other
-- decision types.
data StartTimerDecisionAttributes = StartTimerDecisionAttributes
    { _stdaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    , _stdaTimerId :: Text
      -- ^ The unique Id of the timer. This field is required. The specified
      -- string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control
      -- characters (\u0000-\u001f | \u007f - \u009f). Also, it must not
      -- contain the literal string &quot;arn&quot;.
    , _stdaStartToFireTimeout :: Text
      -- ^ The duration to wait before firing the timer. This field is
      -- required. The duration is specified in seconds. The valid values
      -- are integers greater than or equal to 0.
    } deriving (Show, Generic)

instance FromJSON StartTimerDecisionAttributes

instance ToJSON StartTimerDecisionAttributes

-- | If the event is of type StartTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes
    { _stfeaCause :: StartTimerFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _stfeaTimerId :: Text
      -- ^ The timerId provided in the StartTimer decision that failed.
    , _stfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartTimer decision for this
      -- activity task. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON StartTimerFailedEventAttributes

instance ToJSON StartTimerFailedEventAttributes

-- | If the event is of type TimerCanceled then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes
    { _tceaTimerId :: Text
      -- ^ The unique Id of the timer that was canceled.
    , _tceaStartedEventId :: Integer
      -- ^ The id of the TimerStarted event that was recorded when this
      -- timer was started. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this
      -- event.
    , _tceaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelTimer decision to cancel
      -- this timer. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON TimerCanceledEventAttributes

instance ToJSON TimerCanceledEventAttributes

-- | If the event is of type TimerFired then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerFiredEventAttributes = TimerFiredEventAttributes
    { _tfeaTimerId :: Text
      -- ^ The unique Id of the timer that fired.
    , _tfeaStartedEventId :: Integer
      -- ^ The id of the TimerStarted event that was recorded when this
      -- timer was started. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this
      -- event.
    } deriving (Show, Generic)

instance FromJSON TimerFiredEventAttributes

instance ToJSON TimerFiredEventAttributes

-- | If the event is of type TimerStarted then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerStartedEventAttributes = TimerStartedEventAttributes
    { _tseaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    , _tseaTimerId :: Text
      -- ^ The unique Id of the timer that was started.
    , _tseaStartToFireTimeout :: Text
      -- ^ The duration of time after which the timer will fire. The
      -- duration is specified in seconds. The valid values are integers
      -- greater than or equal to 0.
    , _tseaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartTimer decision for this
      -- activity task. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON TimerStartedEventAttributes

instance ToJSON TimerStartedEventAttributes

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
data WorkflowExecution = WorkflowExecution
    { _weRunId :: Text
      -- ^ A system generated unique identifier for the workflow execution.
    , _weWorkflowId :: Text
      -- ^ The user defined identifier associated with the workflow
      -- execution.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecution

instance ToJSON WorkflowExecution

-- | If the event is of type WorkflowExecutionCancelRequested then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { _wecreaExternalWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The external workflow execution for which the cancellation was
      -- requested.
    , _wecreaExternalInitiatedEventId :: Maybe Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated
      -- event corresponding to the RequestCancelExternalWorkflowExecution
      -- decision to cancel this workflow execution.The source event with
      -- this Id can be found in the history of the source workflow
      -- execution. This information can be useful for diagnosing problems
      -- by tracing back the chain of events leading up to this event.
    , _wecreaCause :: Maybe WorkflowExecutionCancelRequestedCause
      -- ^ If set, indicates that the request to cancel the workflow
      -- execution was automatically generated, and specifies the cause.
      -- This happens if the parent workflow execution times out or is
      -- terminated, and the child policy is set to cancel child
      -- executions.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionCancelRequestedEventAttributes

instance ToJSON WorkflowExecutionCancelRequestedEventAttributes

-- | If the event is of type WorkflowExecutionCanceled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes
    { _wecebDetails :: Maybe Text
      -- ^ Details for the cancellation (if any).
    , _wecebDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelWorkflowExecution
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionCanceledEventAttributes

instance ToJSON WorkflowExecutionCanceledEventAttributes

-- | If the event is of type WorkflowExecutionCompleted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes
    { _weceaResult :: Maybe Text
      -- ^ The result produced by the workflow execution upon successful
      -- completion.
    , _weceaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CompleteWorkflowExecution
      -- decision to complete this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionCompletedEventAttributes

instance ToJSON WorkflowExecutionCompletedEventAttributes

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration
    { _weeTaskStartToCloseTimeout :: Text
      -- ^ The maximum duration allowed for decision tasks for this workflow
      -- execution. The valid values are integers greater than or equal to
      -- 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    , _weeExecutionStartToCloseTimeout :: Text
      -- ^ The total duration for this workflow execution. The valid values
      -- are integers greater than or equal to 0. An integer value can be
      -- used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , _weeTaskList :: TaskList
      -- ^ The task list used for the decision tasks generated for this
      -- workflow execution.
    , _weeChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions if this
      -- workflow execution is terminated, by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired
      -- timeout. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionConfiguration

-- | If the event is of type WorkflowExecutionContinuedAsNew then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes
    { _wecaneaTagList :: [Text]
      -- ^ The list of tags associated with the new workflow execution.
    , _wecaneaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration of decision tasks for the new workflow
      -- execution. The valid values are integers greater than or equal to
      -- 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    , _wecaneaWorkflowType :: WorkflowType
      -- ^ Represents a workflow type.
    , _wecaneaInput :: Maybe Text
      -- ^ The input provided to the new workflow execution.
    , _wecaneaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration allowed for the new workflow execution. The
      -- valid values are integers greater than or equal to 0. An integer
      -- value can be used to specify the duration in seconds while NONE
      -- can be used to specify unlimited duration.
    , _wecaneaTaskList :: TaskList
      -- ^ Represents a task list.
    , _wecaneaChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions of the new
      -- execution if it is terminated by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired
      -- timeout. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _wecaneaNewExecutionRunId :: Text
      -- ^ The runId of the new workflow execution.
    , _wecaneaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the ContinueAsNewWorkflowExecution
      -- decision that started this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionContinuedAsNewEventAttributes

instance ToJSON WorkflowExecutionContinuedAsNewEventAttributes

-- | If the event is of type WorkflowExecutionFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes
    { _wefeaReason :: Maybe Text
      -- ^ The descriptive reason provided for the failure (if any).
    , _wefeaDetails :: Maybe Text
      -- ^ The details of the failure (if any).
    , _wefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the FailWorkflowExecution decision
      -- to fail this execution. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionFailedEventAttributes

instance ToJSON WorkflowExecutionFailedEventAttributes

-- | Contains information about a workflow execution. DescribeWorkflowExecution.
-- -->.
data WorkflowExecutionInfo = WorkflowExecutionInfo
    { _wejParent :: Maybe WorkflowExecution
      -- ^ If this workflow execution is a child of another execution then
      -- contains the workflow execution that started this execution.
    , _wejTagList :: [Text]
      -- ^ The list of tags associated with the workflow execution. Tags can
      -- be used to identify and list workflow executions of interest
      -- through the visibility APIs. A workflow execution can have a
      -- maximum of 5 tags.
    , _wejWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution.
    , _wejExecutionStatus :: ExecutionStatus
      -- ^ The current status of the execution.
    , _wejExecution :: WorkflowExecution
      -- ^ The workflow execution this information is about.
    , _wejCloseStatus :: Maybe CloseStatus
      -- ^ If the execution status is closed then this specifies how the
      -- execution was closed: COMPLETED: the execution was successfully
      -- completed. CANCELED: the execution was canceled.Cancellation
      -- allows the implementation to gracefully clean up before the
      -- execution is closed. TERMINATED: the execution was force
      -- terminated. FAILED: the execution failed to complete. TIMED_OUT:
      -- the execution did not complete in the alloted time and was
      -- automatically timed out. CONTINUED_AS_NEW: the execution is
      -- logically continued. This means the current execution was
      -- completed and a new execution was started to carry on the
      -- workflow.
    , _wejCloseTimestamp :: Maybe POSIX
      -- ^ The time when the workflow execution was closed. Set only if the
      -- execution status is CLOSED.
    , _wejStartTimestamp :: POSIX
      -- ^ The time when the execution was started.
    , _wejCancelRequested :: Maybe Bool
      -- ^ Set to true if a cancellation is requested for this workflow
      -- execution.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionInfo

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts
    { _weocOpenChildWorkflowExecutions :: Integer
      -- ^ The count of child workflow executions whose status is OPEN.
    , _weocOpenActivityTasks :: Integer
      -- ^ The count of activity tasks whose status is OPEN.
    , _weocOpenDecisionTasks :: Integer
      -- ^ The count of decision tasks whose status is OPEN. A workflow
      -- execution can have at most one open decision task.
    , _weocOpenTimers :: Integer
      -- ^ The count of timers started by this workflow execution that have
      -- not fired yet.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionOpenCounts

-- | If the event is of type WorkflowExecutionSignaled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes
    { _wesebExternalWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The workflow execution that sent the signal. This is set only of
      -- the signal was sent by another workflow execution.
    , _wesebExternalInitiatedEventId :: Maybe Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event
      -- corresponding to the SignalExternalWorkflow decision to signal
      -- this workflow execution.The source event with this Id can be
      -- found in the history of the source workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event. This field is set
      -- only if the signal was initiated by another workflow execution.
    , _wesebInput :: Maybe Text
      -- ^ Inputs provided with the signal (if any). The decider can use the
      -- signal name and inputs to determine how to process the signal.
    , _wesebSignalName :: Text
      -- ^ The name of the signal received. The decider can use the signal
      -- name and inputs to determine how to the process the signal.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionSignaledEventAttributes

instance ToJSON WorkflowExecutionSignaledEventAttributes

-- | If the event is of type WorkflowExecutionStarted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes
    { _weseaParentInitiatedEventId :: Maybe Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this workflow execution. The source event with this Id can
      -- be found in the history of the source workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event.
    , _weseaTagList :: [Text]
      -- ^ The list of tags associated with this workflow execution. An
      -- execution can have up to 5 tags.
    , _weseaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration of decision tasks for this workflow type.
      -- The valid values are integers greater than or equal to 0. An
      -- integer value can be used to specify the duration in seconds
      -- while NONE can be used to specify unlimited duration.
    , _weseaWorkflowType :: WorkflowType
      -- ^ The workflow type of this execution.
    , _weseaInput :: Maybe Text
      -- ^ The input provided to the workflow execution (if any).
    , _weseaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this workflow execution. The valid
      -- values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be
      -- used to specify unlimited duration.
    , _weseaTaskList :: TaskList
      -- ^ The name of the task list for scheduling the decision tasks for
      -- this workflow execution.
    , _weseaChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions if this
      -- workflow execution is terminated, by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired
      -- timeout. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _weseaParentWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The source workflow execution that started this workflow
      -- execution. The member is not set if the workflow execution was
      -- not started by a workflow.
    , _weseaContinuedExecutionRunId :: Maybe Text
      -- ^ If this workflow execution was started due to a
      -- ContinueAsNewWorkflowExecution decision, then it contains the
      -- runId of the previous workflow execution that was closed and
      -- continued as this execution.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionStartedEventAttributes

instance ToJSON WorkflowExecutionStartedEventAttributes

-- | If the event is of type WorkflowExecutionTerminated then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes
    { _weteaCause :: Maybe WorkflowExecutionTerminatedCause
      -- ^ If set, indicates that the workflow execution was automatically
      -- terminated, and specifies the cause. This happens if the parent
      -- workflow execution times out or is terminated and the child
      -- policy is set to terminate child executions.
    , _weteaReason :: Maybe Text
      -- ^ The reason provided for the termination (if any).
    , _weteaChildPolicy :: ChildPolicy
      -- ^ The policy used for the child workflow executions of this
      -- workflow execution. The supported child policies are: TERMINATE:
      -- the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by
      -- recording a WorkflowExecutionCancelRequested event in its
      -- history. It is up to the decider to take appropriate actions when
      -- it receives an execution history with this event. ABANDON: no
      -- action will be taken. The child executions will continue to run.
    , _weteaDetails :: Maybe Text
      -- ^ The details provided for the termination (if any).
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionTerminatedEventAttributes

instance ToJSON WorkflowExecutionTerminatedEventAttributes

-- | If the event is of type WorkflowExecutionTimedOut then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes
    { _wetoeaTimeoutType :: WorkflowExecutionTimeoutType
      -- ^ The type of timeout that caused this event.
    , _wetoeaChildPolicy :: ChildPolicy
      -- ^ The policy used for the child workflow executions of this
      -- workflow execution. The supported child policies are: TERMINATE:
      -- the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by
      -- recording a WorkflowExecutionCancelRequested event in its
      -- history. It is up to the decider to take appropriate actions when
      -- it receives an execution history with this event. ABANDON: no
      -- action will be taken. The child executions will continue to run.
    } deriving (Show, Generic)

instance FromJSON WorkflowExecutionTimedOutEventAttributes

instance ToJSON WorkflowExecutionTimedOutEventAttributes

-- | The type of the workflow execution.
data WorkflowType = WorkflowType
    { _wtName :: Text
      -- ^ The name of the workflow type. This field is required. The
      -- combination of workflow type name and version must be unique with
      -- in a domain.
    , _wtVersion :: Text
      -- ^ The version of the workflow type. This field is required. The
      -- combination of workflow type name and version must be unique with
      -- in a domain.
    } deriving (Show, Generic)

instance FromJSON WorkflowType

instance ToJSON WorkflowType

-- | Configuration settings of the workflow type registered through
-- RegisterWorkflowType.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration
    { _wtcDefaultChildPolicy :: Maybe ChildPolicy
      -- ^ The optional default policy to use for the child workflow
      -- executions when a workflow execution of this type is terminated,
      -- by calling the TerminateWorkflowExecution action explicitly or
      -- due to an expired timeout. This default can be overridden when
      -- starting a workflow execution using the StartWorkflowExecution
      -- action or the StartChildWorkflowExecution Decision. The supported
      -- child policies are: TERMINATE: the child executions will be
      -- terminated. REQUEST_CANCEL: a request to cancel will be attempted
      -- for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _wtcDefaultTaskList :: Maybe TaskList
      -- ^ The optional default task list, specified when registering the
      -- workflow type, for decisions tasks scheduled for workflow
      -- executions of this type. This default can be overridden when
      -- starting a workflow execution using the StartWorkflowExecution
      -- action or the StartChildWorkflowExecution Decision.
    , _wtcDefaultExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the workflow type, for executions of this workflow type. This
      -- default can be overridden when starting a workflow execution
      -- using the StartWorkflowExecution action or the
      -- StartChildWorkflowExecution Decision. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , _wtcDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the workflow type, that a decision task for executions of this
      -- workflow type might take before returning completion or failure.
      -- If the task does not close in the specified time then the task is
      -- automatically timed out and rescheduled. If the decider
      -- eventually reports a completion or failure, it is ignored. This
      -- default can be overridden when starting a workflow execution
      -- using the StartWorkflowExecution action or the
      -- StartChildWorkflowExecution Decision. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    } deriving (Show, Generic)

instance FromJSON WorkflowTypeConfiguration

-- | If specified, only executions of the type specified in the filter are
-- returned. executionFilter, typeFilter and tagFilter are mutually exclusive.
-- You can specify at most one of these in a request.
data WorkflowTypeFilter = WorkflowTypeFilter
    { _wtfName :: Text
      -- ^ Name of the workflow type. This field is required.
    , _wtfVersion :: Maybe Text
      -- ^ Version of the workflow type.
    } deriving (Show, Generic)

instance ToJSON WorkflowTypeFilter

-- | Contains information about a workflow type.
data WorkflowTypeInfo = WorkflowTypeInfo
    { _wtjStatus :: RegistrationStatus
      -- ^ The current status of the workflow type.
    , _wtjWorkflowType :: WorkflowType
      -- ^ The workflow type this information is about.
    , _wtjDeprecationDate :: Maybe POSIX
      -- ^ If the type is in deprecated state, then it is set to the date
      -- when the type was deprecated.
    , _wtjCreationDate :: POSIX
      -- ^ The date when this type was registered.
    , _wtjDescription :: Maybe Text
      -- ^ The description of the type registered through
      -- RegisterWorkflowType.
    } deriving (Show, Generic)

instance FromJSON WorkflowTypeInfo

makeLenses ''CancelTimerDecisionAttributes
makeLenses ''CancelWorkflowExecutionDecisionAttributes
makeLenses ''CloseStatusFilter
makeLenses ''CompleteWorkflowExecutionDecisionAttributes
makeLenses ''DomainConfiguration
makeLenses ''RequestCancelActivityTaskDecisionAttributes
makeLenses ''TagFilter
makeLenses ''TaskList
makeLenses ''WorkflowExecutionFilter
makeLenses ''ActivityTaskCancelRequestedEventAttributes
makeLenses ''ActivityTaskCanceledEventAttributes
makeLenses ''ActivityTaskCompletedEventAttributes
makeLenses ''ActivityTaskFailedEventAttributes
makeLenses ''ActivityTaskScheduledEventAttributes
makeLenses ''ActivityTaskStartedEventAttributes
makeLenses ''ActivityTaskTimedOutEventAttributes
makeLenses ''ActivityType
makeLenses ''ActivityTypeConfiguration
makeLenses ''ActivityTypeInfo
makeLenses ''CancelTimerFailedEventAttributes
makeLenses ''CancelWorkflowExecutionFailedEventAttributes
makeLenses ''ChildWorkflowExecutionCanceledEventAttributes
makeLenses ''ChildWorkflowExecutionCompletedEventAttributes
makeLenses ''ChildWorkflowExecutionFailedEventAttributes
makeLenses ''ChildWorkflowExecutionStartedEventAttributes
makeLenses ''ChildWorkflowExecutionTerminatedEventAttributes
makeLenses ''ChildWorkflowExecutionTimedOutEventAttributes
makeLenses ''CompleteWorkflowExecutionFailedEventAttributes
makeLenses ''ContinueAsNewWorkflowExecutionDecisionAttributes
makeLenses ''ContinueAsNewWorkflowExecutionFailedEventAttributes
makeLenses ''Decision
makeLenses ''DecisionTaskCompletedEventAttributes
makeLenses ''DecisionTaskScheduledEventAttributes
makeLenses ''DecisionTaskStartedEventAttributes
makeLenses ''DecisionTaskTimedOutEventAttributes
makeLenses ''DomainInfo
makeLenses ''ExecutionTimeFilter
makeLenses ''ExternalWorkflowExecutionCancelRequestedEventAttributes
makeLenses ''ExternalWorkflowExecutionSignaledEventAttributes
makeLenses ''FailWorkflowExecutionDecisionAttributes
makeLenses ''FailWorkflowExecutionFailedEventAttributes
makeLenses ''HistoryEvent
makeLenses ''MarkerRecordedEventAttributes
makeLenses ''RecordMarkerDecisionAttributes
makeLenses ''RecordMarkerFailedEventAttributes
makeLenses ''RequestCancelActivityTaskFailedEventAttributes
makeLenses ''RequestCancelExternalWorkflowExecutionDecisionAttributes
makeLenses ''RequestCancelExternalWorkflowExecutionFailedEventAttributes
makeLenses ''RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
makeLenses ''ScheduleActivityTaskDecisionAttributes
makeLenses ''ScheduleActivityTaskFailedEventAttributes
makeLenses ''SignalExternalWorkflowExecutionDecisionAttributes
makeLenses ''SignalExternalWorkflowExecutionFailedEventAttributes
makeLenses ''SignalExternalWorkflowExecutionInitiatedEventAttributes
makeLenses ''StartChildWorkflowExecutionDecisionAttributes
makeLenses ''StartChildWorkflowExecutionFailedEventAttributes
makeLenses ''StartChildWorkflowExecutionInitiatedEventAttributes
makeLenses ''StartTimerDecisionAttributes
makeLenses ''StartTimerFailedEventAttributes
makeLenses ''TimerCanceledEventAttributes
makeLenses ''TimerFiredEventAttributes
makeLenses ''TimerStartedEventAttributes
makeLenses ''WorkflowExecution
makeLenses ''WorkflowExecutionCancelRequestedEventAttributes
makeLenses ''WorkflowExecutionCanceledEventAttributes
makeLenses ''WorkflowExecutionCompletedEventAttributes
makeLenses ''WorkflowExecutionConfiguration
makeLenses ''WorkflowExecutionContinuedAsNewEventAttributes
makeLenses ''WorkflowExecutionFailedEventAttributes
makeLenses ''WorkflowExecutionInfo
makeLenses ''WorkflowExecutionOpenCounts
makeLenses ''WorkflowExecutionSignaledEventAttributes
makeLenses ''WorkflowExecutionStartedEventAttributes
makeLenses ''WorkflowExecutionTerminatedEventAttributes
makeLenses ''WorkflowExecutionTimedOutEventAttributes
makeLenses ''WorkflowType
makeLenses ''WorkflowTypeConfiguration
makeLenses ''WorkflowTypeFilter
makeLenses ''WorkflowTypeInfo
