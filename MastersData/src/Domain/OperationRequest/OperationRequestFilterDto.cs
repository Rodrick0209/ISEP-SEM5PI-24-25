using System;
using Microsoft.VisualBasic;

namespace DDDSample1.Domain.OperationRequest
{
    public class OperationRequestFilterDto
    {
        
        public string? OperationType { get; set; }
        public string? PatientName { get; set; }
        public string? MedicalRecordNumber { get; set; }
        public DateTime? StartDate { get; set; }
        public DateTime? EndDate { get; set; }

    }
}

