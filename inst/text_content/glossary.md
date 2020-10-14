The glossary is divided into two sections, "General terms" which has definitions for terms used across several parts of the model, and “Definitions of nodes” which covers the specific definitions developed for each risk node identified within our overall network of digital preservation risk, and the states that each node can take as part of the model.

<hr>

# General terms

## Bitstream

The stored set of binary information that comprises each file making up a digital object. Bits (*bi*nary digi*ts*) can be represented by 0 and 1 when presenting binary information. On storage this will be physically implemented by differing orientations of very small magnetic particles (hard drives and tapes), the presence or absence of a pit in the surface of a CD or DVD (or a different colour in the dye layer of a CD-R or DVD-R) or on/off voltage values in flash storage such as memory sticks.

## Born-digital

Digital material that has only ever existed as digital objects. This includes material such as spreadsheets, word-processor documents, original digital photos from phones and digital cameras (however, if the image was of an analogue archival document that would be a surrogate). Unless an archive is able to specify that they will only accept certain file formats for accessioning born-digital material can be very variable in terms of the range of file formats held within the archive, making the ongoing processes of digital preservation more complex. Archivists and other digital preservation specialists will require a greater range of technical skills and tools in order to be able to understand and maintain the files.

## Digital material

A general term covering the three distinct types of digital material recognised in the risk model: born digital, digitised records and surrogates.

## Digital object

An individual logical piece of digital material that the archive is preserving. Typically an individual digital file or set of digital files that must be preserved together in order to meaningfully preserve that piece of digital material.

## Digitised record

Many of the considerations of surrogate material also apply to digitised records, such as the narrower range of file formats expected. However, in this case the original analogue material is not retained, perhaps because it has been damaged in certain ways that make it impossible to preserve (for example still image negatives or film stock that has been affected by vinegar syndrome which causes irreversible damage, and can also spread to other similar materials within the archive). This also means that the digitised version cannot subsequently be compared back to the record so additional metadata may be captured during the digitisation process in order to fully reflect the provenance of the images and capture information about information already lost from the record by the damage suffered up to that point.

## Model

DiAGRAM is built on an underlying general model of digital preservation processes in the form of a Bayesian network, connecting various risk nodes via probabilities of occurrence conditional on other risks. In order to make a useful baseline model for your own archive you need to add input data that specifies particular aspects of the digital material you currently hold, and the systems and processes you have in place now. This initial baseline model will show your current risk levels. Using DiAGRAM you can then add scenarios to investigate the effect of various potential changes to your archive’s digital preservation processes and policies in order to compare the impact on your risk prevention score to help you determine the most effective strategy in your own case.

## Node

Each risk to digital material identified during the development of DiAGRAM is represented by a node (displayed as an oval shape) in the Bayesian network, the arrows linking nodes show how risks influence each other, reducing or increasing the related risks.

## Scenario

Following the creation of the initial model of your own archive you can create scenarios by adjusting the input values you used in order to model possible future states of your archive. For example, how would your risk prevention score change if you were able to provide training to all your staff to improve their technical skills. Alternatively, if you focussed all your efforts on improving your information management around the digital archive, would that give you an even greater improvement. If you then add appropriate costing information to these potential interventions you can begin to build a business case to implement the desired changes.

## State

Each node will have a few possible states. These represent the different options it can be. For instance, the technical skills node has two states, good and poor, and the digital object node has three states, born-digital, digitised and surrogate.

## Surrogate

The typical output of a digitisation project, usually in the form of a digital image file (such as tiff, jpeg or jpeg2000), or a PDF file. If digitising audiovisual material this could also include digital video or audio files. However, the archive retains the original analogue material (on paper, parchment, film etc) which remains the original archival record. The surrogate provides more convenient access without risk of damaging the original (microfilm or facsimile copies are also forms of surrogate). Also, since the archive can influence the digitisation process and the file formats created there will typically be a narrower range of file formats found in archives of surrogate material compared to born-digital material. Also, because the original analogue material remains in the archive, it is possible to redigitise if surrogate materials are themselves damaged or lost (though there would obviously be a cost in doing this). Therefore you might be prepared to tolerate somewhat higher risks to surrogates than to unique born digital materials. If users of the archive have questions about the completeness of capture in a surrogate digitisation project, it will be possible to produce the original record.

<hr>

# Definitions of Nodes

## Bit Preservation

**Definition**: A term used to describe a very basic level of preservation of digital resource files as  they were received from the depositor (literally preservation of the bits forming that form a digital resource). Activities may include maintaining onsite and offsite backup copies, virus checking, fixity-checking, and periodic refreshment to new storage media.

**Data source**: Deterministic mostly but also gathered from the structured expert judgement workshop.

**Data collected**: April 2020

<table>
  <thead>
    <tr>
      <td>Node State</td>
      <td>State Definition</td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Yes</td>
      <td>The bitstream is preserved.</td>
    </tr>
    <tr>
      <td>No</td>
      <td>The bitstream is not preserved.</td>
    </tr>
  </tbody>
</table>


## Checksum

**Definition**: A unique numerical signature derived from a file that can be used to compare copies (definition from the DPC <a href = "https://www.dpconline.org/handbook/glossary" target="_blank">handbook</a>.). A checksum is needed to ensure integrity of the digital object.

**Data source**: Loosely based on JISC digital skills survey results.

**Data collected**: 2019

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We have a checksum from the depositor.</td>
  </tr>
  <tr>
    <td>Archivist-generated</td>
    <td>We do not have a checksum from the depositor but we generated one ourselves when we received the material.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>You don't have checksums at all, or they were generated sometime after initial receipt.</td>
  </tr>
  </tbody>
</table>


## Conditions of Use

**Definition**: Knowing of the conditions of use and any restrictions on the digital material, including the legal status, copyright, who owns the intellectual property and Freedom of Information restrictions.

**Data source**: gathered from the structured expert judgement workshop

**Data collected**: April 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We know the conditions of use and any restrictions on the digital material.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We do not know the conditions of use of the digital material.</td>
  </tr>
  </tbody>
</table>


## Content Metadata

**Definition**: Describes the intellectual entity through properties such as author and title, and supports discovery and delivery of digital content. It may also provide an historic context, by, for example, specifying which print-based material was the original source for a digital derivative (source provenance). It also includes provenance information of who has cared for the digital object and what preservation actions have been performed on it. This definition is adapted from the <a href = "https://www.loc.gov/standards/premis/FE_Dappert_Enders_MetadataStds_isqv22no2.pdf" target="_blank">Digital Preservation Metadata Standards publication</a>.

**Data source**: gathered from the structured expert judgement workshop

**Data collected**: April 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>The content metadata is sufficient and meets all our requirements.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>The content metadata is insufficient and does not meet all our requirements.</td>
  </tr>
  </tbody>
</table>


## Digital Object

**Definition**: The proportion of your archive made up of born-digital, digitised and surrogate files.

**Data source**: Loosely based on JISC digital skills survey results and TNA data

**Data collected**: 2019

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Born_digital</td>
    <td>Records were created in a digital format.</td>
  </tr>
  <tr>
    <td>Digitised</td>
    <td>Records have been created as a result of converting analogue originals, but you do not hold those originals.</td>
  </tr>
  <tr>
    <td>Surrogate</td>
    <td>Digital images have been created as a result of converting analogue originals, and you also hold those originals. </td>
  </tr>
  </tbody>
</table>


## File format

**Definition**: A file format is a standard way that information is encoded for storage in a computer file. It tells the computer how to display, print, and process, and save the information. It is dictated by the application program which created the file, and the operating system under which it was created and stored (definition from <a href = "http://en.wikipedia.org/wiki/File_format" target="_blank">wikipedia</a>). The file format can be described as ubiquitous if it is widely known and used by non-specialists. If the file format is not proprietary, it can be described as open. The availability of tools and software to render a digital object depends on the file format.

**Data source**: TNA data and JISC digital skills survey results

**Data collected**: Jan 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>The file formats are open and/or ubiquitous.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>The file formats are proprietary and not widely used.</td>
  </tr>
  </tbody>
</table>


## Identity

**Definition**: Knowing what the material is and where it is from. Specifically... Can you locate the file? Is it sufficiently described for you to know this is what you want? Can you understand its context within the archive? Can you find other versions of the file which were created by preservation actions? Can you find the provenance of the file?

**Data source**: Deterministic mostly but includes data gathered from the structured expert judgement workshop too

**Data collected**: April 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We know what the material is and where it is from.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We do not know what the material is and where it is from.</td>
  </tr>
  </tbody>
</table>


## Information Management

**Definition**: Internal systems and support for coherent information management and documentation of preservation actions. This is needed to ensure integrity and provenance of the digital object.

**Data source**: Loosely based on JISC digital skills survey results

**Data collected**: 2019

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Sufficient</td>
    <td>We have sufficient internal information management systems to track the preservation actions taken on a digital object since acquisition.</td>
  </tr>
  <tr>
    <td>Insufficient</td>
    <td>We have insufficient internal information management systems to track the preservation actions carried out on a digital object since acquisition.</td>
  </tr>
  </tbody>
</table>


## Integrity

**Definition**: The assurance that the bit-stream is identical to when it was added to the archive.

**Data source**: Elicit conditional probabilities with some deterministic results

**Data collected**: April 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We know that the material has not been changed, aside from any deliberate preservation actions.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We do not know whether the material has been changed, or we know that it has been changed unexpectedly and is not restorable.</td>
  </tr>
  </tbody>
</table>


## Intellectual Control

**Definition**: Having full knowledge of the material content, provenance and conditions of use.

**Data source**: Deterministic

**Data collected**: 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We have full knowledge of the material content, provenance and conditions of use.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We do not have full knowledge of the material content, provenance and conditions of use.</td>
  </tr>
  </tbody>
</table>


## Obsolescence

**Definition**: Essential equipment, hardware or software required to access the bit stream becoming out of date or unusable. For example, hardware no longer being produced or essential software to access the media no longer supported.

**Data source:** **gathered from the structured expert judgement workshop** and cloud data storage service level agreements

**Data collected**: April 2020

<table>
  <thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We cannot access the bit stream.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We can access the bit stream.</td>
  </tr>
  </tbody>
</table>


## Operating Environment

**Definition**: **Your archive's policy on the storage location of your digital material.**

**Data source**: Loosely based on JISC digital skills survey results

**Data collected**: 2019

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We have a copy stored in a different geographical location or if not, we have adequate mitigation strategies in place at our storage location in case of a flood.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We store all our digital material in the same geographical location and do not have adequate mitigation strategies in place in case of a flood.</td>
  </tr>
</tbody><table>


## Physical Disaster

**Definition**: **The risk of a flood at your archive's primary storage location.** For this first version of the tool, we will only consider a flood as a physical disaster.

**Data source**: Gov.uk flood checker

**Data collected**: 2020

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>There is a flood at the primary storage location.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>There is no flood at the primary storage location.</td>
  </tr>
</tbody><table>


## Renderability

**Definition**: The object is a sufficiently useful representation of the original file.

**Data source**: Deterministic mostly but includes data gathered from the structured expert judgement workshop too

**Data collected**: April 2020

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>The object can be rendered to provide a sufficiently useful representation of the original file.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>The object cannot be rendered to provide a sufficiently useful representation of the original file.</td>
  </tr>
</tbody><table>


## Replication and Refreshment

**Definition**: **Your archive's policies on making copies and regularly moving digital material on to newer versions of the storage media.** 

**Data source**: Loosely based on JISC digital skills survey results

**Data collected**: 2019

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Good</td>
    <td>We always have at least two copies of the bitstream at any time and the bitstream is copied to newer versions of the storage media regularly to avoid corruption due to aging media. </td>
  </tr>
  <tr>
    <td>Poor</td>
    <td>We do not have copies of the material or we have copies but we do not have a refreshment policy to ensure that there will always be at least one additional copy if another became damaged or lost.</td>
  </tr>
</tbody><table>


## Storage Life

**Definition**: The length of time for which the physical storage device is expected to store the digital object's bit stream. The end of storage life can be described as the point at which you can no longer store or retrieve data due to hardware defects or malfunction.

**Data source:** **gathered from the structured expert judgement workshop** and cloud data storage service level agreements

**Data collected**: April 2020

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Yes</td>
    <td>We expect the bit-stream to be stored, unaltered by the storage medium, for at least 12 months.</td>
  </tr>
  <tr>
    <td>No</td>
    <td>We expect the bit-stream to be altered or lost due to the storage medium within 12 months.</td>
  </tr>
</tbody><table>


## Storage Medium

**Definition**: **The type of media on which your digital material is stored such as USB hard drives, CDs or the Cloud.** 

**Data source**: Estimates

**Data collected**: 2020

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>A</td>
    <td>Expected lifespan below 10 years or unknown, highly susceptible to physical damage, requires specific environmental conditions and very sensitive to changes, does not support error-detection methods, supporting technology is novel, proprietary and limited. Examples include USB flash drives (memory sticks), floppy disks, SD drives and CD-R discs.</td>
  </tr>
  <tr>
    <td>B</td>
    <td>A proven lifespan of at least 10 years, low susceptibility to physical damage, tolerant of a wide range of environmental conditions without data loss, supports robust error-detection methods, supporting technology is well established and widely available. Examples include LTO tapes, Blu-ray discs, enterprise/corporate managed hard drives and CD-ROM discs.</td>
  </tr>
  <tr>
    <td>C</td>
    <td>An external company is responsible for our digital storage. Examples include Amazon Simple Storage Service, Microsoft Azure Archive Storage and Google Cloud Storage.</td>
  </tr>
</tbody><table>


## System Security

**Definition**: **A secure system can protect data from deletion or modification from any unauthorized party, and it ensures that when an authorized person makes a change that should not have been made, the damage can be reversed. Definition from <a href = "https://www.forcepoint.com/cyber-edu/cia-triad" target="_blank">Forcepoint.</a>**

**Data source**: JISC digital skills survey results

**Data collected**: 2019

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Good</td>
    <td>We have a good security system, that is we actively manage access restrictions both virtually and physically, we have up-to-date antivirus software, we perform regular audits, etc.</td>
  </tr>
  <tr>
    <td>Poor</td>
    <td>We have a poor security system, that is we do not actively manage access restrictions, we do not have up-to-date antivirus software, we do not perform audits, etc.</td>
  </tr>
</tbody><table>


## Technical Metadata

**Definition**: Technical information that describes how a file functions and that enables a computer to understand it at the bit level, so that it can be rendered in a way that is faithful to its original content. Definition adapted from <a href = "https://www.nedcc.org/fundamentals-of-av-preservation-textbook/chapter-4-introduction/chapter-4-section-5" target="_blank">here</a>.

**Data source:** **gathered from the structured expert judgement workshop**

**Data collected**: April 2020

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Sufficient</td>
    <td>The technical metadata documentation is sufficient - we have an identification report, and if necessary, further validation tools have also been used.</td>
  </tr>
  <tr>
    <td>Insufficient</td>
    <td>The technical metadata documentation is insufficient - we do not have enough information documented to ensure that the file can be rendered in a way that is faithful to its original content.</td>
  </tr>
</tbody><table>


## Technical Skills 

**Definition:** **Bespoke digital preservation skills such as awareness of technological trends, detailed knowledge of storage media, hardware and software, skills to perform file format migration, skills to find emulating software etc?**

**Data source**: JISC digital skills survey results

**Data collected**: 2019

<table><thead>
  <tr>
    <td>Node State</td>
    <td>State Definition</td>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td>Good</td>
    <td>We have the capability to perform actions such as file format migration, software emulation and data recovery, as well as the ability to investigate and manipulate data, and basic coding skills.We have the capability to perform actions such as file format migration, software emulation and data recovery, as well as the ability to investigate and manipulate data, and basic coding skills.</td>
  </tr>
  <tr>
    <td>Poor</td>
    <td>We do not have the capability to perform actions such as file format migration, software emulation and data recovery, nor the ability to investigate and manipulate data.</td>
  </tr>
</tbody><table>


## Tools to Render

**Definition:** Availability of tools and software to render the digital material and the expertise to use them.

**Data source:** **gathered from the structured expert judgement workshop**

**Data collected:** April 2020

|||
|--- |--- |
|Node State|State Definition|
|Yes|We have the tools and expertise to render a file of this type.|
|No|We do not have the tools and expertise to render a file of this type.|

