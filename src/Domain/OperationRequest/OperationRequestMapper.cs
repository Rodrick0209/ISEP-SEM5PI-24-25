


using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequestMapper
    {

        public static OperationRequestDto toDTO(OperationRequest obj)
        {
            return new OperationRequestDto (obj.Id.AsGuid(), obj.deadLineDate.ToString(), obj.priority.ToString(), obj.patientId, obj.operationTypeId);
        }

        public static OperationRequest toDomain(OperationRequestDto obj)
        {
            return new OperationRequest(obj.DeadLineDate, obj.Priority, obj.PatientId, obj.OperationTypeId);
        }









    }









}