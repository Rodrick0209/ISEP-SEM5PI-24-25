


using DDDSample1.Domain.StaffLoggers;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.StaffLoggers
{

    public class StaffLoggerMapper
    {

        public static StaffLoggerDto toDTO(StaffLogger staff)
        {
            return new StaffLoggerDto(staff.Id.AsString(), staff.LicenseNumber, staff.SpecializationId, staff.AvailabilitySlotsId, staff.Email, staff.PhoneNumber, staff.Category, staff.LoggerType, staff.ModificationDate);
        }
    }
}


