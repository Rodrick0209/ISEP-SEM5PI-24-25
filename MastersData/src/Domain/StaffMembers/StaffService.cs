using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Families;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.Availability;
using DDDSample1.Domain.Specializations;
using System;



namespace DDDSample1.Domain.StaffMembers
{
    public class StaffService
    {
    private readonly IUnitOfWork _unitOfWork;

    private readonly IStaffRepository _staffRepository;

    

        public StaffService(IUnitOfWork unitOfWork, IStaffRepository staffRepository)
        {
            _unitOfWork = unitOfWork;
            _staffRepository = staffRepository;
            
        }


      /*  public async Task<StaffDto> AddAync(Staff staff)
        {


            bool emailIsUnique = await validateEmailIsUnique(staff.Email.email);
            bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(staff.PhoneNumber.phoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            LicenseNumber licenseNumber = new LicenseNumber(staff.LicenseNumber.licenseNumber);
            Category category = staff.Category;
            FullName fullName = new FullName(staff.FullName.fullName);
            Email email = new Email(staff.Email.email);
            DateTime recruitmentDate = DateTime.Now; 
            PhoneNumber phoneNumber = new PhoneNumber(staff.PhoneNumber.phoneNumber);

           
            StaffId staffId = staffIdGeneratorService.generateStaffId(category, recruitmentDate);
           

            
            
            


            var staffMember = new Staff(
                fullName,
                
                gender,
                email,
                phoneNumber,
                emergencyContact,
                medicalRecordNumber
            );

            await _patientRepository.AddAsync(patient);
            await _unitOfWork.CommitAsync();

            return PatientMapper.ToDto(patient);
        }*/

         private async Task<bool> validateEmailIsUnique(string email)
        {
            var existingStaff = await _staffRepository.GetByEmailAsync(email);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> validatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingStaff = await _staffRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }




}
}