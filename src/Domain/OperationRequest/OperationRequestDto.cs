using System;
using DDDSample1.Domain.OperationType;
using DDDSample1.Domain.Patient;




namespace DDDSample1.Domain.OperationRequest
{


    public class OperationRequestDto
    {
        public Guid Id { get; set; }

        public string DeadLineDate { get; set; }

        public string Priority { get; set; }

        public PatientId PatientId { get; set; }

        public OperationTypeId OperationTypeId { get; set; }

        public OperationRequestDto(Guid id, string deadLineDate, string priority, PatientId patientId, OperationTypeId operationTypeId)
        {
            this.Id = id;
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;
        }
        




    }






}