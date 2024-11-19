using System;
using System.Collections.Generic;



namespace DDDSample1.Domain.OperationRequest
{
    public class StaffAssignedSurgeryDto
    {
        public List<String> staffAnesthesyPhase { get; private set; }
        public List<String> staffSurgeryPhase { get; private set; }
    
        public StaffAssignedSurgeryDto(List<String> staffAnesthesyPhase, List<String> staffSurgeryPhase)
        {
            this.staffAnesthesyPhase = staffAnesthesyPhase;
            this.staffSurgeryPhase = staffSurgeryPhase;
        }
       
    }



}