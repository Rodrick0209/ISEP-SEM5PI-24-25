


using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequestMapper
    {

        public static OperationRequestDto toDTO(OperationRequest obj)
        {
            return new OperationRequestDto (obj.Id.AsGuid(), obj.deadLineDate.deadLineDate, obj.priority.priority, obj.patientId, obj.operationTypeId, obj.doctorThatRequestedId,obj.doctorThatWillPerformId);
        }

        public static OperationRequest toDomain(OperationRequestDto obj, string doctorThatRequestedId)
        {
            return new OperationRequest(obj.DeadLineDate, obj.Priority, obj.PatientId, obj.OperationTypeId,doctorThatRequestedId, obj.DoctorThatWillPerformId);
        }









    }









}