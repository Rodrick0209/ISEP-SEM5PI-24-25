
using System;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRequestLoggers
{

    public class OperationRequestLogger : Entity<OperationRequestLoggerId>,IAggregateRoot
    {
        
        public OperationRequestId OperationRequestId { get; private set; }
        public string Priority { get; private set; }
        public string PatientId { get; private set; }
        public string OperationTypeId { get; private set; }
        public string DoctorId { get; private set; }
        public string DeadLineDate { get; private set; } // Added missing property

        private OperationRequestLogger()
        {
        }

        public OperationRequestLogger(string deadLineDate, string priority, string operationTypeId, string doctorId, OperationRequestId operationRequestId)
        {
            this.OperationRequestId = operationRequestId;
            this.Priority = priority;
            this.DeadLineDate = deadLineDate; // Fixed property name and added missing semicolon
            this.OperationTypeId = operationTypeId;
            this.DoctorId = doctorId;
        }



    }
}