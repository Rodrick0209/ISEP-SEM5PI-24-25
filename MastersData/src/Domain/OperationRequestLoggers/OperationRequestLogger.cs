
using System;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRequestLoggers
{

    public class OperationRequestLogger : Entity<OperationRequestLoggerId>,IAggregateRoot
    {
        
        public string OperationRequestId { get; private set; }
        public string Priority { get; private set; }
        public string OperationTypeId { get; private set; }
        public string DoctorThatRequestedId { get; private set; }
        
        public string DoctorThatWillPerformId { get; private set; }
        public string DeadLineDate { get; private set; } 

        public DateTime CreatedAt { get; private set; } = DateTime.Now;

        public string LoggerType { get; private set; }

        private OperationRequestLogger()
        {
        }

        public OperationRequestLogger(string deadLineDate, string priority, string operationTypeId, string doctorThatWillPerformId ,string doctorThatRequestedId, string operationRequestId,string loggerType)
        {
            this.Id = new OperationRequestLoggerId(Guid.NewGuid());
            this.OperationRequestId = operationRequestId;
            this.Priority = priority;
            this.DeadLineDate = deadLineDate;
            this.OperationTypeId = operationTypeId;
            this.DoctorThatRequestedId = doctorThatRequestedId;
            this.DoctorThatWillPerformId = doctorThatWillPerformId;
            this.LoggerType = loggerType;
            this.CreatedAt = DateTime.Now;
        }



    }
}