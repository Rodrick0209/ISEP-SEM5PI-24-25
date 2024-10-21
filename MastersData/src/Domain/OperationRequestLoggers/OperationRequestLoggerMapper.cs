


using DDDSample1.Domain.OperationRequestLoggers;

namespace DDDSample1.Domain.OperationRequestLoggers
{

    public class OperationRequestLoggerMapper
    {

        public static OperationRequestLoggerDto toDTO(OperationRequestLogger op)
        {
            return new OperationRequestLoggerDto(op.Id.AsString(),op.DeadLineDate,op.Priority,op.OperationTypeId,op.DoctorThatRequestedId,op.DoctorThatWillPerformId,op.OperationRequestId,op.LoggerType);
        }
    }
}

       
