using System;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMapper
    {
        public static StaffDto toDTO(Staff obj)
        {
            return new StaffDto(
                obj.Id,
                obj.FullName.fullName,
                obj.LicenseNumber.licenseNumber,
                obj.SpecializationId,
                obj.AvailabilitySlotsId,
                obj.Email.email,
                obj.PhoneNumber.phoneNumber,
                obj.Category.ToString(),
                obj.status.ToString()
            );
        }

        public static Staff toDomain(StaffDto dto, StaffId staffId)
        {
            return new Staff(
                staffId,
                dto.FullName,
                dto.LicenseNumber,
                dto.SpecializationId,
                dto.AvailabilitySlotsId,
                dto.Email,
                dto.PhoneNumber,
                dto.Category,
                dto.status  
            );
        }
    }
}
