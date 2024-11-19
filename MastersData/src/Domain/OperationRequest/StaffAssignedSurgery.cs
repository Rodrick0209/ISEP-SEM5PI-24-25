using DDDSample1.Domain.Shared;
using System;
using System.Collections.Generic;



namespace DDDSample1.Domain.OperationRequest
{

    public class StaffAssignedSurgery : Entity<StaffAssignedSurgeryId>
    {

        public List<String> staffAnesthesyPhase { get; private set; }
        public List<String> staffSurgeryPhase { get; private set; }

        private StaffAssignedSurgery()
        {
        }


        public StaffAssignedSurgery(List<String> staffAnesthesyPhase, List<String> staffSurgeryPhase)
        {
            this.Id = new StaffAssignedSurgeryId(Guid.NewGuid());
            this.staffAnesthesyPhase = staffAnesthesyPhase;
            this.staffSurgeryPhase = staffSurgeryPhase;
        }
        
        public void addStaffAnesthesyPhase(String staffId)
        {
            this.staffAnesthesyPhase.Add(staffId);
        }

        public void addStaffSurgeryPhase(String staffId)
        {
            this.staffSurgeryPhase.Add(staffId);
        }




    }

}