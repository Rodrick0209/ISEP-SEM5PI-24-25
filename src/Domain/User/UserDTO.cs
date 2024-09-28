using System;



namespace DDDSample1.Domain.User
{
    public class UserDTO
    {

        public Guid Id { get; set; }
        public string role { get; set; }
        public string username { get; set; }
        public string email { get; set; }



        public UserDTO(Guid Id, string role, string username, string email)
        {
            this.Id = Id;
            this.role = role;
            this.username = username;
            this.email = email;
        }





    } 


}