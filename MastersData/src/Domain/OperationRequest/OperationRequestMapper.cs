


using System;
using DDDSample1.Domain.OperationRequest;
using Microsoft.AspNetCore.JsonPatch.Operations;

namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequestMapper
    {

        public static OperationRequestDto toDTO(OperationRequest obj)
        {
            return new OperationRequestDto(obj.Id.AsGuid(), obj.deadLineDate.deadLineDate, obj.priority.priority, obj.patientId, obj.operationTypeId, obj.doctorThatRequestedId, obj.doctorThatWillPerformId);
        }

        public static OperationRequestDto toDtoForUI(OperationRequest obj, string patientMedicalRecordNumber, string operationTypeName)
        {
            return new OperationRequestDto(obj.Id.AsGuid(), obj.deadLineDate.deadLineDate, obj.priority.priority, patientMedicalRecordNumber, operationTypeName, obj.doctorThatRequestedId, obj.doctorThatWillPerformId);
        }

        public static OperationRequest toDomain(OperationRequestDto obj, string doctorThatRequestedId)
        {
            Console.WriteLine("DOUTOR MAPPER ->" + doctorThatRequestedId);
            OperationRequest opRequest = new OperationRequest(obj.DeadLineDate, obj.Priority, obj.PatientId, obj.OperationTypeId, doctorThatRequestedId, obj.DoctorThatWillPerformId);
            Console.WriteLine ("Depois do mapper-->", opRequest.doctorThatRequestedId);
            return opRequest;

        }


        public static OperationRequest toDomain(OperationRequestDto obj)
        {
            OperationRequest opRequest = new OperationRequest(obj.DeadLineDate, obj.Priority, obj.PatientId, obj.OperationTypeId, obj.DoctorThatRequestedId, obj.DoctorThatWillPerformId);
            Console.WriteLine ("Depois do mapper-->", opRequest.doctorThatRequestedId);
            return opRequest;

        }









    }









}