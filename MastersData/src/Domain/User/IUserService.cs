using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.User;

public interface IUserService
{
    Task<string> GetLogToken(LoginRequest request);
    Task<User> AddAsync(User user);
    Task<List<User>> GetAllAsync();
    Task<User> GetByEmailAsync(string email);
    Task<string> sendEmailWithUrlResetPassword(string email);
    Task<string> ResetPassword(User user, string newPassword, string token);
    Task<ConfirmationPatientDto> RegisterPatientAsync(RegisteringPatientDto dto);
    Task<UserDTO> ConfirmRegisterPatientAsync(ConfirmationPatientDto dto);
    Task<ConfirmationEditPatientDto> EditPatientAsync(EditingPatientDto dto);
    Task<UserDTO> ConfirmEditPatientAsync(ConfirmationEditPatientDto dto);
    Task<ConfirmationPatientDto> DeletePatientAsync(string email);
    Task ConfirmDeletePatientAsync(ConfirmationPatientDto dto);
    Task<string> GenerateGoogleTokenFromJwt(string email, string role);
    Task<UserDTO> GetByIdAsync(UserId id);
    Task<UserDTO> GetByEmailSearchAsync(string email);
}