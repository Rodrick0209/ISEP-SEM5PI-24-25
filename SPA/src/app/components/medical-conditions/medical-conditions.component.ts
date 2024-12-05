import { Component } from '@angular/core';
import { ListMedicalConditionsComponent } from '../list-medical-conditions/list-medical-conditions.component';

@Component({
  selector: 'app-medical-conditions',
  standalone: true,
  imports: [ListMedicalConditionsComponent],
  templateUrl: './medical-conditions.component.html',
  styleUrl: './medical-conditions.component.css'
})
export class MedicalConditionsComponent {

}
