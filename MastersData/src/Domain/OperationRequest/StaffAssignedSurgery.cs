using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;
using System;
using System.Collections.Generic;



namespace DDDSample1.Domain.OperationRequest
{

    public class StaffAssignedSurgery : Entity<StaffAssignedSurgeryId>
    {

        public List<StaffId> staffAnesthesyPhase { get; private set; }
        public List<StaffId> staffSurgeryPhase { get; private set; }

        private StaffAssignedSurgery() { }


        public StaffAssignedSurgery(List<StaffId> staffAnesthesyPhase, List<StaffId> staffSurgeryPhase)
        {
            this.Id = new StaffAssignedSurgeryId(Guid.NewGuid());
            this.staffAnesthesyPhase = staffAnesthesyPhase;
            this.staffSurgeryPhase = staffSurgeryPhase;
        }
        
        public void addStaffAnesthesyPhase(StaffId staffId)
        {
            this.staffAnesthesyPhase.Add(staffId);
        }

        public void addStaffSurgeryPhase(StaffId staffId)
        {
            this.staffSurgeryPhase.Add(staffId);
        }




    }

}