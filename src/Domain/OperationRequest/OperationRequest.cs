using System;
using DDDSample1.Domain.OperationType;
using DDDSample1.Domain.Patient;
using DDDSample1.Domain.Shared; 



namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequest : Entity<OperationRequestId>,IAggregateRoot
    {

        public DeadLineDate deadLineDate { get; private set; }
        public Priority priority { get; private set; }

        public PatientId patientId { get; private set; }

        public OperationTypeId operationTypeId { get; private set; }   


        private OperationRequest()
        {
        }

        public OperationRequest(string deadLineDate, string priority, PatientId patientId, OperationTypeId operationTypeId)
        {
            this.Id = new OperationRequestId(Guid.NewGuid());
            this.priority = new Priority(priority);
            this.deadLineDate = new DeadLineDate(deadLineDate);
            this.patientId = patientId;
            this.operationTypeId = operationTypeId;

        }








    }







}