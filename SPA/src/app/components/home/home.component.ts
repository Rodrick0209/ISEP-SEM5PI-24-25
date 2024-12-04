import { Component } from '@angular/core';
import { AuthService } from '../../services/auth.service';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './home.component.html',
  styleUrl: './home.component.css'
})
export class HomeComponent {

  constructor(private authService: AuthService, private router: Router){ }

  extractRole(): any {
    return this.authService.extractRoleFromToken();
  }

  extractEmail(): any {
    return this.authService.extractEmailFromToken();
  }

  isAdmin(): boolean {
    const role = this.extractRole();
    return role === 'admin';
  }

  isDoctor(): boolean {
    const role = this.extractRole();
    return role === 'doctor';
  }

  isPatient(): boolean {
    const role = this.extractRole();
    return role === 'patient';
  }

  isNewUserOrNotRegistered(): boolean {
    const role = this.extractRole();
    return role == null;
  }

  scheduleSurgeries() {
    this.router.navigate(['/planning']);
  }

  managePatients() {
    this.router.navigate(['/patients']);
  }

  manageOperationTypes() {
    this.router.navigate(['/operationType']);
  }

  manageStaffs() {
    this.router.navigate(['/staffs']);
  }

  manageSpecializations() {
    this.router.navigate(['/specializations'])
  }

  addRoomTypes(){
    this.router.navigate(['/room-types/add'])
  }

  manageOperationRequests(){
    this.router.navigate(['/operationRequests'])
  }

  managePatientRecords(){
    // Not Implemented yet
  }

  viewProfile(){
    const email = this.extractEmail();
    this.router.navigate(['/profile', `${email}`])
  }

  downloadPatientRecord(){
    // Not implemented yet
  }

}
