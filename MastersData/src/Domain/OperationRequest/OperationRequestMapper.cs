


using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequestMapper
    {

        public static OperationRequestDto toDTO(OperationRequest obj)
        {
            return new OperationRequestDto (obj.Id.AsGuid(), obj.deadLineDate.deadLineDate, obj.priority.priority, obj.patientId, obj.operationTypeId, obj.doctorId);
        }

        public static OperationRequest toDomain(OperationRequestDto obj)
        {
            return new OperationRequest(obj.DeadLineDate, obj.Priority, obj.PatientId, obj.OperationTypeId, obj.DoctorId);
        }









    }









}